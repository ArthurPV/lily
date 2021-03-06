module Diagnostic = Lily_lexer.Diagnostic
module Lexer = Lily_lexer.Lexer
module Location = Lily_lexer.Location
open Lily_lexer.Token
open Ast

type parser = {
  lexer : Lexer.lexer;
  mutable pos : int;
  mutable nodes : (ast * Location.location) array;
  mutable errors : Diagnostic.diagnostic array;
  mutable current_token : token;
  mutable previous_token : token;
  mutable current_location : Location.location;
  mutable previous_location : Location.location;
}

let ast_of_nodes parser ~idx = match parser.nodes.(idx) with n, _ -> n
let loc_of_nodes parser ~idx = match parser.nodes.(idx) with _, l -> l

let collect_public_nodes nodes =
  let rec loop ?(i = 0) ?(pub = []) () =
    if i < Array.length nodes then
      match nodes.(i) with
      | ( Decl (Fun { is_pub; _ }), _
        | Decl (Constant { is_pub; _ }), _
        | Decl (Module { is_pub; _ }), _
        | Decl (Alias { is_pub; _ }), _
        | Decl (Record { is_pub; _ }), _
        | Decl (Enum { is_pub; _ }), _
        | Decl (Error { is_pub; _ }), _
        | Decl (Class { is_pub; _ }), _
        | Decl (Trait { is_pub; _ }), _
        | Decl (Import { is_pub; _ }), _ ) as node
        when is_pub ->
          loop ~i:(i + 1) ~pub:(node :: pub) ()
      | _ -> loop ~i:(i + 1) ~pub ()
    else pub |> List.rev |> Array.of_list
  in
  loop ()

let change_nodes_visibility nodes ~visibility =
  Array.map
    (fun x ->
      match x with
      | ( Decl
            (Fun
              {
                id;
                poly_args;
                args;
                return_type;
                body;
                is_async;
                is_test;
                is_export;
                _;
              }),
          l ) ->
          ( Decl
              (Fun
                 {
                   id;
                   poly_args;
                   args;
                   return_type;
                   body;
                   is_pub = visibility;
                   is_async;
                   is_test;
                   is_export;
                 }),
            l )
      | Decl (Constant { id; data_type; expr; _ }), l ->
          (Decl (Constant { id; data_type; expr; is_pub = visibility }), l)
      | Decl (Module { id; body; is_test; _ }), l ->
          (Decl (Module { id; body; is_pub = visibility; is_test }), l)
      | Decl (Alias { id; poly_args; data_type; _ }), l ->
          (Decl (Alias { id; poly_args; data_type; is_pub = visibility }), l)
      | Decl (Record { id; poly_args; fields; _ }), l ->
          (Decl (Record { id; poly_args; fields; is_pub = visibility }), l)
      | Decl (Enum { id; poly_args; variants; _ }), l ->
          (Decl (Enum { id; poly_args; variants; is_pub = visibility }), l)
      | Decl (Error { id; poly_args; variant; _ }), l ->
          (Decl (Error { id; poly_args; variant; is_pub = visibility }), l)
      | Decl (Class { id; poly_args; inh; body; _ }), l ->
          (Decl (Class { id; poly_args; inh; body; is_pub = visibility }), l)
      | Decl (Trait { id; poly_args; body; _ }), l ->
          (Decl (Trait { id; poly_args; body; is_pub = visibility }), l)
      | Decl (Import { import; _as; _ }), l ->
          (Decl (Import { import; is_pub = visibility; _as }), l)
      | _ -> failwith "unreachable")
    nodes

let new_parser lexer =
  Lexer.run lexer;
  {
    lexer;
    pos = 0;
    nodes = [||];
    errors = [||];
    current_token = Lexer.tok_of_tokens lexer ~idx:0;
    previous_token = Lexer.tok_of_tokens lexer ~idx:0;
    current_location = Lexer.loc_of_tokens lexer ~idx:0;
    previous_location = Lexer.loc_of_tokens lexer ~idx:0;
  }

let new_diagnostic kind msg loc = Diagnostic.new_diagnostic ~msg kind loc

let advance parser ~add_pos =
  if add_pos && parser.pos < Array.length parser.lexer.tokens - 1 then
    parser.pos <- parser.pos + 1;
  parser.current_token <- Lexer.tok_of_tokens parser.lexer ~idx:parser.pos;
  parser.current_location <- Lexer.loc_of_tokens parser.lexer ~idx:parser.pos;
  match Lexer.tok_of_tokens parser.lexer ~idx:(parser.pos - 1) with
  | Comment _ -> ()
  | _ ->
      parser.previous_location <-
        Lexer.loc_of_tokens parser.lexer ~idx:(parser.pos - 1);
      parser.previous_token <-
        Lexer.tok_of_tokens parser.lexer ~idx:(parser.pos - 1)

let next_token parser =
  (match Lexer.tok_of_tokens parser.lexer ~idx:parser.pos with
  | Comment (Doc s) ->
      parser.nodes <-
        Array.append parser.nodes
          [| (Doc s, Lexer.loc_of_tokens parser.lexer ~idx:parser.pos) |];
      advance parser ~add_pos:true
  | _ -> advance parser ~add_pos:true);
  match parser.current_token with
  | Comment _ ->
      let rec loop () =
        if parser.pos < Array.length parser.lexer.tokens - 1 then
          match Lexer.tok_of_tokens parser.lexer ~idx:parser.pos with
          | Comment (Doc s) ->
              parser.nodes <-
                [|
                  (Doc s, Lexer.loc_of_tokens parser.lexer ~idx:parser.pos);
                |]
                |> Array.append parser.nodes;
              parser.pos <- parser.pos + 1;
              loop ()
          | Comment _ ->
              parser.pos <- parser.pos + 1;
              loop ()
          | _ -> advance ~add_pos:false parser
      in
      loop ()
  | _ -> ()

let expect_token parser tok err =
  (match parser.current_token with
  | Comment _ -> next_token parser
  | _ -> ());
  if tok <> parser.current_token then raise err else next_token parser

let matches parser tok =
  (match parser.current_token with
  | Comment _ -> next_token parser
  | _ -> ());
  if parser.current_token = tok then (
    next_token parser;
    true)
  else false

let is_eof parser =
  match parser.current_token with Separator Eof -> true | _ -> false

let peek_token parser ~n =
  if parser.pos + n < Array.length parser.lexer.tokens - 1 then
    Some (Lexer.tok_of_tokens parser.lexer ~idx:(parser.pos + n))
  else None

let rec parse_data_type parser =
  next_token parser;
  match parser.previous_token with
  | Identifier "Int8" -> `I8
  | Identifier "Int16" -> `I16
  | Identifier "Int32" -> `I32
  | Identifier "Int64" -> `I64
  | Identifier "Int128" -> `I128
  | Identifier "Uint8" -> `U8
  | Identifier "Uint16" -> `U16
  | Identifier "Uint32" -> `U32
  | Identifier "Uint64" -> `U64
  | Identifier "Uint128" -> `U128
  | Identifier "Float32" -> `F32
  | Identifier "Float64" -> `F64
  | Identifier "Char" -> `Char
  | Identifier "String" -> `String
  | Identifier "Usize" -> `Usize
  | Identifier "Isize" -> `Isize
  | Identifier "Bool" -> `Bool
  | Identifier "Unit" -> `Unit
  | Keyword Self -> `SelfArg
  | Identifier s when String.lowercase_ascii s = s -> `Generics s
  | Identifier s -> `CustomType (s, None) (* TODO: add type argument *)
  | Separator Bar -> parse_fun_data_type parser
  | Separator LeftHook ->
      if is_data_type parser ~n:0 then (
        let dt = parse_data_type parser in
        Diagnostic.EmitDiagnostic
          ( show_token parser.current_token
            |> Printf.sprintf "expected `]`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator RightHook);
        `Array dt)
      else
        Diagnostic.EmitDiagnostic
          ( show_token parser.current_token
            |> Printf.sprintf "expected data type, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  | Separator LeftParen ->
      if is_data_type parser ~n:0 then (
        let rec loop ?(dts = [||]) () =
          if
            parser.current_token <> Separator RightParen
            && parser.current_token <> Separator Eof
          then (
            let dt = parse_data_type parser in
            if parser.current_token <> Separator RightParen then
              Diagnostic.EmitDiagnostic
                ( show_token parser.current_token
                  |> Printf.sprintf "expected `,`, found `%s`",
                  Diagnostic.Error,
                  parser.current_location )
              |> expect_token parser (Separator Comma);
            loop ~dts:(Array.append dts [| dt |]) ())
          else dts
        in
        let dts = loop () in
        Diagnostic.EmitDiagnostic
          ( show_token parser.current_token
            |> Printf.sprintf "expected `)`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator RightParen);
        `Tuple dts)
      else
        Diagnostic.EmitDiagnostic
          ( show_token parser.current_token
            |> Printf.sprintf "expected data type, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  | _ ->
      Diagnostic.EmitDiagnostic
        ( show_token parser.previous_token
          |> Printf.sprintf "expected data type, found `%s`",
          Diagnostic.Error,
          parser.previous_location )
      |> raise

and parse_fun_data_type parser =
  let rec loop ?(items = []) () =
    if parser.current_token <> Separator Bar then (
      let dt = parse_data_type parser in
      if parser.current_token <> Separator Bar then
        Diagnostic.EmitDiagnostic
          ( show_token parser.current_token
            |> Printf.sprintf "expected `->`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Arrow);
      loop ~items:(dt :: items) ())
    else (
      next_token parser;
      `Fun
        ( List.length items - 1
          |> Array.sub (items |> List.rev |> Array.of_list) 0,
          List.nth items 0 ))
  in
  loop ()

and is_binop parser ~n =
  match peek_token ~n parser with
  | Some (Operator Plus)
  | Some (Operator Minus)
  | Some (Operator Star)
  | Some (Operator Slash)
  | Some (Operator Percentage)
  | Some (Operator Hat)
  | Some (Operator PlusPlus)
  | Some (Operator MinusMinus)
  | Some (Operator PlusEq)
  | Some (Operator MinusEq)
  | Some (Operator StarEq)
  | Some (Operator SlashEq)
  | Some (Operator PercentageEq)
  | Some (Operator HatEq)
  | Some (Operator Eq)
  | Some (Operator EqEq)
  | Some (Operator DotDot)
  | Some (Operator LeftShift)
  | Some (Operator RightShift)
  | Some (Operator LeftShiftEq)
  | Some (Operator RightShiftEq)
  | Some (Operator BangEq)
  | Some (Operator Interrogation) ->
      true
  | _ -> false

and is_data_type parser ~n =
  match peek_token ~n parser with
  | Some (Identifier _) | Some (Separator Bar) | Some (Keyword Self) -> true
  | (Some (Separator LeftHook) | Some (Separator LeftParen))
    when is_data_type parser ~n:(n + 1) ->
      true
  | _ -> false

[@@@warning "-27"]
[@@@warning "-39"]

let convert_in_integer (tok : token) =
  match tok with
  | Literal (Int s) -> (
      try Literal (Int32 (s |> Stdint.Int32.of_string))
      with Invalid_argument _ -> (
        try Literal (Int64 (s |> Stdint.Int64.of_string))
        with Invalid_argument _ -> (
          try Literal (Int128 (s |> Stdint.Int128.of_string))
          with Invalid_argument _ -> failwith "too long")))
  | _ -> failwith "unreachable"

let verify_if_is_tuple parser =
  let rec loop ?(n = 0) () =
    match peek_token parser ~n with
    | ( Some (Separator LeftParen)
      | Some (Separator LeftHook)
      | Some (Separator LeftBrace) ) as sep ->
        let inv_sep =
          match sep with
          | Some (Separator LeftParen) -> Some (Separator RightParen)
          | Some (Separator LeftHook) -> Some (Separator RightHook)
          | Some (Separator LeftBrace) -> Some (Separator RightBrace)
          | _ -> failwith "unrechable"
        in
        let rec loop2 ?(n = n + 1) ?(count = 1) ?(find = 0) () =
          if count <> find then
            match peek_token parser ~n with
            | s when s = sep -> loop2 ~n:(n + 1) ~count:(count + 1) ~find ()
            | s when s = inv_sep ->
                loop2 ~n:(n + 1) ~count ~find:(find + 1) ()
            | None -> failwith "found eof"
            | _ -> loop2 ~n:(n + 1) ~count ~find ()
          else n
        in
        loop ~n:(loop2 ()) ()
    | Some (Separator Comma) -> true
    | Some (Separator RightParen) -> false
    | None -> failwith "found eof"
    | _ -> loop ~n:(n + 1) ()
  in
  loop ()

let rec run parser =
  (* skip comemnt if is first *)
  (match parser.current_token with
  | Comment _ -> next_token parser
  | _ -> ());
  if parser.pos < Array.length parser.lexer.tokens - 1 then (
    let loc = Location.copy_location parser.current_location in
    try
      let node = parse_decl parser in
      Location.end_location loc parser.previous_location;
      parser.nodes <- [| (node, loc) |] |> Array.append parser.nodes;
      if parser.current_token <> Separator Eof then
        if
          parser.previous_location.line = parser.current_location.line
          |> Bool.not
        then ()
        else
          parser.errors <-
            [|
              Location.copy_location parser.previous_location
              |> new_diagnostic Diagnostic.Error
                   "unexpected end of expression";
            |]
            |> Array.append parser.errors
      else ();
      run parser
    with Diagnostic.EmitDiagnostic (msg, kind, loc) ->
      parser.errors <-
        [| Location.copy_location loc |> new_diagnostic kind msg |]
        |> Array.append parser.errors;
      next_token parser;
      run parser)
  else (
    parser.errors |> Array.iter (fun x -> Diagnostic.emit_diagnostic x);
    if Array.length parser.errors > 0 then exit 1)

and parse_assign parser loc =
  let node = parse_logical_or parser in
  if
    matches parser (Operator PlusEq)
    || matches parser (Operator MinusEq)
    || matches parser (Operator StarEq)
    || matches parser (Operator SlashEq)
    || matches parser (Operator HatEq)
    || matches parser (Operator PercentageEq)
    || matches parser (Operator Eq)
  then
    let id =
      match node with
      | Identifier (id, _) -> Identifier (id, None)
      | IdentifierAccess (ids, _) -> IdentifierAccess (ids, None)
      | SelfAccess (ids, _) -> SelfAccess (ids, None)
      | _ ->
          Location.end_location loc parser.current_location;
          Diagnostic.EmitDiagnostic
            ( "the left-hand assignment take an identifier",
              Diagnostic.Error,
              loc )
          |> raise
    in
    match parser.previous_token with
    | Operator PlusEq -> AddAssign (id, parse_assign parser loc)
    | Operator MinusEq -> SubAssign (id, parse_assign parser loc)
    | Operator StarEq -> MulAssign (id, parse_assign parser loc)
    | Operator SlashEq -> DivAssign (id, parse_assign parser loc)
    | Operator PercentageEq -> ModAssign (id, parse_assign parser loc)
    | Operator HatEq -> ExpAssign (id, parse_assign parser loc)
    | Operator Eq -> Assign (id, parse_assign parser loc)
    | _ -> failwith "unreachable"
  else node

and parse_logical_or parser =
  let node = parser |> parse_logical_and |> ref in
  let rec loop () =
    if matches parser (Keyword Or) then (
      node := Or (!node, parse_logical_and parser);
      loop ())
  in
  loop ();
  !node

and parse_logical_and parser =
  let node = parser |> parse_equality |> ref in
  let rec loop () =
    if matches parser (Keyword And) then (
      node := And (!node, parse_equality parser);
      loop ())
  in
  loop ();
  !node

and parse_equality parser =
  let node = parser |> parse_comparison |> ref in
  let rec loop () =
    if matches parser (Operator EqEq) || matches parser (Operator BangEq)
    then (
      (match parser.previous_token with
      | Operator EqEq -> node := Eq (!node, parse_comparison parser)
      | Operator BangEq -> node := Ne (!node, parse_comparison parser)
      | _ -> failwith "unreachable");
      loop ())
  in
  loop ();
  !node

and parse_comparison parser =
  let node = parser |> parse_range |> ref in
  let rec loop () =
    if
      matches parser (Operator LeftShift)
      || matches parser (Operator RightShift)
      || matches parser (Operator LeftShiftEq)
      || matches parser (Operator RightShiftEq)
    then (
      (match parser.previous_token with
      | Operator LeftShift -> node := Lt (!node, parse_range parser)
      | Operator RightShift -> node := Gt (!node, parse_range parser)
      | Operator LeftShiftEq -> node := Le (!node, parse_range parser)
      | Operator RightShiftEq -> node := Ge (!node, parse_range parser)
      | _ -> failwith "unreachable");
      loop ())
  in
  loop ();
  !node

and parse_range parser =
  let node = parser |> parse_term |> ref in
  let rec loop () =
    if matches parser (Operator DotDot) then (
      node := Range (!node, parse_term parser);
      loop ())
  in
  loop ();
  !node

and parse_term parser =
  let node = parser |> parse_factor |> ref in
  let rec loop () =
    if matches parser (Operator Plus) || matches parser (Operator Minus) then (
      (match parser.previous_token with
      | Operator Plus -> node := Add (!node, parse_factor parser)
      | Operator Minus -> node := Sub (!node, parse_factor parser)
      | _ -> failwith "unreachable");
      loop ())
  in
  loop ();
  !node

and parse_factor parser =
  let node = parser |> parse_exp |> ref in
  let rec loop () =
    if
      matches parser (Operator Star)
      || matches parser (Operator Slash)
      || matches parser (Operator Percentage)
    then (
      (match parser.previous_token with
      | Operator Star -> node := Mul (!node, parse_exp parser)
      | Operator Slash -> node := Div (!node, parse_exp parser)
      | Operator Percentage -> node := Mod (!node, parse_exp parser)
      | _ -> failwith "unreachable");
      loop ())
  in
  loop ();
  !node

and parse_exp parser =
  let node = parser |> parse_unary |> ref in
  let rec loop () =
    if matches parser (Operator Hat) then (
      node := Exp (!node, parse_unary parser);
      loop ())
  in
  loop ();
  !node

and parse_unary parser =
  if
    matches parser (Operator Plus)
    || matches parser (Operator Minus)
    || matches parser (Keyword Not)
  then
    match parser.previous_token with
    | Operator Plus -> Positive (parse_unary parser)
    | Operator Minus -> Negative (parse_unary parser)
    | Keyword Not -> Not (parse_unary parser)
    | _ -> failwith "unrechable"
  else parse_primary_expr parser

and parse_grouping parser ~is_mut =
  if parser.current_token = Separator RightParen then (
    next_token parser;
    Expr (Literal Unit))
  else
    let i = ref 0 in
    let rec loop () =
      if
        peek_token parser ~n:!i <> Some (Separator RightParen)
        && peek_token parser ~n:!i <> None
      then (
        i := !i + 1;
        loop ())
    in
    loop ();
    if peek_token parser ~n:(!i + 1) = Some (Operator ColonEq) then (
      let ids = parse_multiple parser in
      if match ids with _, cond -> cond then (
        let vars =
          parse_multiple_variable parser
            ~ids:(match ids with t, _ -> t)
            ~is_mut
        in
        let copy_loc = Location.copy_location parser.current_location in
        (* same location for all vars *)
        let rec push_vars ?(i = 0) () =
          if i < Array.length vars - 1 then (
            parser.nodes <-
              Array.append parser.nodes [| (vars.(i), copy_loc) |];
            push_vars ~i:(i + 1) ())
        in
        push_vars ();
        if Array.length vars = 0 then vars.(0)
        else vars.(Array.length vars - 1))
      else
        let consts =
          parse_multiple_constant parser
            ~ids:(match ids with t, _ -> t)
            ~is_pub:false
        in
        let copy_loc = Location.copy_location parser.current_location in
        (* same location for all consts *)
        let rec push_consts ?(i = 0) () =
          if i < Array.length consts - 1 then (
            parser.nodes <-
              Array.append parser.nodes [| (consts.(i), copy_loc) |];
            push_consts ~i:(i + 1) ())
        in
        push_consts ();
        if Array.length consts = 0 then consts.(0)
        else consts.(Array.length consts - 1))
    else
      let expr = parse_expr2 parser in
      Diagnostic.EmitDiagnostic
        ( parser.current_token |> show_token
          |> Printf.sprintf "expected `)`, found `%s`",
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator RightParen);
      Expr expr

and parse_function_call parser ~id =
  next_token parser;
  (* next_token parser; *)
  let rec loop ?(args = []) () =
    if parser.current_token <> Separator RightParen then (
      match parser.current_token with
      | Identifier s when peek_token parser ~n:1 = Some (Operator Eq) ->
          next_token parser;
          next_token parser;
          let expr = Expr (parse_expr2 parser) in
          if parser.current_token <> Separator RightParen then
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "expected `,`, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> expect_token parser (Separator Comma);
          loop ~args:((Some s, expr) :: args) ()
      | _ ->
          let expr = Expr (parse_expr2 parser) in
          if parser.current_token <> Separator RightParen then
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "expected `,`, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> expect_token parser (Separator Comma);
          loop ~args:((None, expr) :: args) ())
    else (
      next_token parser;
      FunctionCall (id, args |> List.rev |> Array.of_list))
  in
  loop ()

and parse_class_call parser =
  let id =
    match parser.current_token with
    | Identifier s when peek_token parser ~n:1 = Some (Separator Dot) ->
        let rec loop ?(l = []) () =
          if parser.current_token <> Separator LeftParen then (
            let id =
              match parser.current_token with
              | Identifier s -> Identifier (s, None)
              | _ ->
                  Diagnostic.EmitDiagnostic
                    ( parser.current_token |> show_token
                      |> Printf.sprintf "expected identifier, found `%s`",
                      Diagnostic.Error,
                      parser.current_location )
                  |> raise
            in
            next_token parser;
            if parser.current_token = Separator Dot then (
              next_token parser;
              loop ~l:(id :: l) ())
            else IdentifierAccess (l |> List.rev |> Array.of_list, None))
          else IdentifierAccess (l |> List.rev |> Array.of_list, None)
        in
        loop ()
    | Identifier s ->
        next_token parser;
        Identifier (s, None)
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected identifier, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  if parser.current_token <> Separator LeftParen then
    Diagnostic.EmitDiagnostic
      ( parser.current_token |> show_token
        |> Printf.sprintf "expected `(`, found `%s`",
        Diagnostic.Error,
        parser.current_location )
    |> raise;
  next_token parser;
  let rec loop ?(args = []) () =
    if parser.current_token <> Separator RightParen then (
      let expr = Expr (parse_expr2 parser) in
      if parser.current_token <> Separator RightParen then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `,`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Comma);
      loop ~args:(expr :: args) ())
    else (
      next_token parser;
      ClassCall (id, args |> List.rev |> Array.of_list))
  in
  loop ()

and parse_record_call parser ~id =
  next_token parser;
  let rec loop ?(args = []) () =
    if parser.current_token <> Separator RightBrace then (
      let id =
        match parser.current_token with
        | Identifier s -> s
        | _ ->
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "expected field name, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> raise
      in
      next_token parser;
      if parser.current_token = Separator Comma then (
        next_token parser;
        loop ~args:((id, None) :: args) ())
      else if parser.current_token = Separator RightBrace then
        loop ~args:((id, None) :: args) ()
      else if parser.current_token = Operator Eq then (
        next_token parser;
        let expr = parse_expr2 parser in
        if parser.current_token <> Separator RightBrace then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        loop ~args:((id, Some expr) :: args) ())
      else
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "unexpected expression: %s",
            Diagnostic.Error,
            parser.current_location )
        |> raise)
    else (
      next_token parser;
      RecordCall (id, args |> List.rev |> Array.of_list))
  in
  loop ()

and parse_anonymous_function parser =
  let args = parse_argument parser in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `=>`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator FatArrow);
  let body = parse_body parser ~closure:(Some (Keyword End), None, None) in
  AnonymousFunction (args, body)

and parse_identifier_access parser f_id =
  next_token parser;
  let rec loop ?(access = [ f_id ]) () =
    match parser.current_token with
    | Identifier s when peek_token parser ~n:1 = Some (Separator LeftParen)
      ->
        (* TODO: review the code to see if there are any other bugs. *)
        let access_ref = ref access in
        access_ref := [ Identifier (s, None) ] @ !access_ref;
        next_token parser;
        let call =
          parse_function_call parser
            ~id:
              (IdentifierAccess
                 (!access_ref |> List.rev |> Array.of_list, None))
        in
        if parser.current_token = Separator Dot then (
          next_token parser;
          loop ~access:(call :: access) ()
          (*loop ~access: ((match call with | FunctionCall (e, vals) ->
            FunctionCall (Identifier (s, None), vals) | _ -> failwith
            "unreachable") :: access) ()*))
        else call
    | Identifier s when peek_token parser ~n:1 = Some (Separator LeftBrace)
      ->
        let access_ref = ref access in
        access_ref := [ Identifier (s, None) ] @ !access_ref;
        next_token parser;
        let call =
          parse_record_call parser
            ~id:
              (IdentifierAccess
                 (!access_ref |> List.rev |> Array.of_list, None))
        in
        if parser.current_token = Separator Dot then (
          next_token parser;
          loop ~access:(call :: access) ())
        else call
    | Identifier s when peek_token parser ~n:1 = Some (Separator LeftHook) ->
        let access_ref = ref access in
        access_ref := [ Identifier (s, None) ] @ !access_ref;
        next_token parser;
        let acc =
          parse_array_access parser
            ~id:
              (IdentifierAccess
                 (!access_ref |> List.rev |> Array.of_list, None))
        in
        if parser.current_token = Separator Dot then (
          next_token parser;
          loop ~access:(acc :: access) ())
        else acc
    | Identifier s
      when peek_token parser ~n:1 = Some (Separator Dot)
           && peek_token parser ~n:2 = Some (Separator LeftParen) ->
        let access_ref = ref access in
        access_ref := [ Identifier (s, None) ] @ !access_ref;
        next_token parser;
        let acc =
          parse_tuple_access parser
            ~id:
              (IdentifierAccess
                 (!access_ref |> List.rev |> Array.of_list, None))
        in
        if parser.current_token = Separator Dot then (
          next_token parser;
          loop ~access:(acc :: access) ())
        else acc
    | Identifier s when peek_token parser ~n:1 = Some (Separator Dot) ->
        next_token parser;
        next_token parser;
        loop ~access:(Identifier (s, None) :: access) ()
    | Identifier s when peek_token parser ~n:1 = Some (Separator Colon) ->
        let access_ref = ref access in
        access_ref := [ Identifier (s, None) ] @ !access_ref;
        next_token parser;
        let call =
          parse_variant parser
            ~id:
              (IdentifierAccess
                 (!access_ref |> List.rev |> Array.of_list, None))
        in
        call
    | Identifier s ->
        next_token parser;
        IdentifierAccess
          ( access @ [ Identifier (s, None) ] |> List.rev |> Array.of_list,
            None )
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "unexpected expression: `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  loop ()

(* TODO: review this function *)
and parse_self_access parser =
  if parser.current_token = Separator Dot then
    match peek_token parser ~n:1 with
    | Some (Identifier s)
      when peek_token parser ~n:2 = Some (Separator Dot)
           || peek_token parser ~n:2 = Some (Separator LeftHook)
           || peek_token parser ~n:2 = Some (Separator LeftParen) -> (
        match parse_identifier_access parser (Identifier (s, None)) with
        | IdentifierAccess (arr, op) -> SelfAccess (arr, op)
        | FunctionCall (IdentifierAccess (arr, op), args) ->
            FunctionCall (SelfAccess (arr, op), args)
        | TupleAccess (IdentifierAccess (arr, op), v) ->
            TupleAccess (SelfAccess (arr, op), v)
        | ArrayAccess (IdentifierAccess (arr, op), v) ->
            ArrayAccess (SelfAccess (arr, op), v)
        | _ -> failwith "unreachable")
    | Some (Identifier s) ->
        next_token parser;
        next_token parser;
        SelfAccess ([| Identifier (s, None) |], None)
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected identifier, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  else Self

and parse_tuple parser =
  if verify_if_is_tuple parser then
    let rec loop ?(tuple = []) () =
      if parser.current_token <> Separator RightParen then (
        let expr = parse_expr2 parser in
        if parser.current_token <> Separator RightParen then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        loop ~tuple:(expr :: tuple) ())
      else (
        next_token parser;
        Tuple (tuple |> List.rev |> Array.of_list))
    in
    loop ()
  else
    let expr = parse_expr2 parser in
    next_token parser;
    Grouping expr

and parse_array parser =
  let rec loop ?(arr = []) () =
    if parser.current_token <> Separator RightHook then (
      let expr = parse_expr2 parser in
      if parser.current_token <> Separator RightHook then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `,`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Comma);
      loop ~arr:(expr :: arr) ())
    else (
      next_token parser;
      Array (arr |> List.rev |> Array.of_list))
  in
  loop ()

and parse_array_access parser ~id =
  let rec loop ?(acc = []) () =
    if parser.current_token = Separator LeftHook then (
      next_token parser;
      let expr = parse_expr2 parser in
      Diagnostic.EmitDiagnostic
        ( parser.current_token |> show_token
          |> Printf.sprintf "expected `]`, found `%s`",
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator RightHook);
      loop ~acc:(expr :: acc) ())
    else acc |> List.rev |> Array.of_list
  in
  ArrayAccess (id, loop ())

and parse_tuple_access parser ~id =
  let rec loop ?(acc = []) () =
    if
      parser.current_token = Separator Dot
      && peek_token parser ~n:1 = Some (Separator LeftParen)
    then (
      next_token parser;
      next_token parser;
      let expr = parse_expr2 parser in
      Diagnostic.EmitDiagnostic
        ( parser.current_token |> show_token
          |> Printf.sprintf "expected `)`, found `%s`",
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator RightParen);
      loop ~acc:(expr :: acc) ())
    else acc |> List.rev |> Array.of_list
  in
  TupleAccess (id, loop ())

and parse_variant parser ~id =
  if matches parser (Separator Colon) then
    let expr = parse_expr2 parser in
    Variant (id, Some expr)
  else if matches parser (Separator ColonColon) then Variant (id, None)
  else
    Diagnostic.EmitDiagnostic
      ( parser.current_token |> show_token
        |> Printf.sprintf "expected `:` or `::`, found `%s`",
        Diagnostic.Error,
        parser.current_location )
    |> raise

and parse_primary_expr parser =
  let loc = Location.copy_location parser.current_location in
  next_token parser;
  let expr =
    match parser.previous_token with
    | Keyword True -> Literal (Bool true)
    | Keyword False -> Literal (Bool false)
    | Literal (Char c) -> Literal (Char c)
    | Literal (Int i) -> convert_in_integer parser.previous_token
    | Literal (Float f) -> Literal (Float (Float.of_string f))
    | Literal (String s) -> Literal (String s)
    | Identifier "_" -> Wildcard
    | Identifier s when String.lowercase_ascii s = s -> (
        match parser.current_token with
        | Separator LeftParen ->
            parse_function_call parser ~id:(Identifier (s, None))
        | Separator LeftHook ->
            parse_array_access parser ~id:(Identifier (s, None))
        | Separator Dot
          when peek_token parser ~n:1 = Some (Separator LeftParen) ->
            parse_tuple_access parser ~id:(Identifier (s, None))
        | Separator Dot ->
            parse_identifier_access parser (Identifier (s, None))
        | _ -> Identifier (s, None))
    | Identifier s when Char.uppercase_ascii s.[0] = s.[0] -> (
        match parser.current_token with
        | Separator Dot
          when peek_token parser ~n:1 = Some (Separator LeftParen) ->
            parse_tuple_access parser ~id:(Identifier (s, None))
        | Separator Dot ->
            parse_identifier_access parser (Identifier (s, None))
        | Separator LeftBrace ->
            parse_record_call parser ~id:(Identifier (s, None))
        | Separator LeftParen ->
            parse_function_call parser ~id:(Identifier (s, None))
        | Separator LeftHook ->
            parse_array_access parser ~id:(Identifier (s, None))
        | Separator Colon | Separator ColonColon ->
            parse_variant parser ~id:(Identifier (s, None))
        | _ -> Identifier (s, None))
    | Identifier s -> Identifier (s, None)
    | Keyword New -> parse_class_call parser
    | Keyword Fun -> parse_anonymous_function parser
    | Keyword Undef -> Undef
    | Keyword Nil -> Nil
    | Keyword Self -> parse_self_access parser
    | Separator LeftHook -> parse_array parser
    | Separator LeftParen when parser.current_token = Separator RightParen ->
        next_token parser;
        Literal Unit
    | Separator LeftParen -> parse_tuple parser
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "unexpected expression: %s",
            Diagnostic.Error,
            parser.previous_location )
        |> raise
  in
  Location.end_location loc parser.current_location;
  expr

and parse_expr2 parser =
  let loc = Location.copy_location parser.current_location in
  let expr = parse_assign parser loc in

  match expr with
  | Grouping _ ->
      Diagnostic.new_diagnostic ~msg:"unnecessary parentheses on expr"
        Diagnostic.Warning loc
      |> Diagnostic.emit_diagnostic;
      expr
  | _ -> expr

and parse_variable parser ~id ~is_mut =
  if parser.current_token <> Operator ColonEq then (
    if parser |> is_data_type ~n:0 |> Bool.not then
      Expr (Identifier (id, None))
    else
      let dt = Some (parse_data_type parser) in
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf
               "expected `:=`, found `%s` in variable named `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Operator ColonEq);
      Decl
        (Variable { id; expr = parse_expr2 parser; data_type = dt; is_mut }))
  else (
    Diagnostic.EmitDiagnostic
      ( id
        |> Printf.sprintf "expected `:=`, found `%s` in variable named `%s`"
             (show_token parser.current_token),
        Diagnostic.Error,
        parser.current_location )
    |> expect_token parser (Operator ColonEq);
    Decl
      (Variable { id; expr = parse_expr2 parser; data_type = None; is_mut }))

and parse_constant parser ~id ~is_pub =
  (* if parser |> is_data_type ~n:0 |> Bool.not then Expr (Identifier (id,
     None)) else *)
  let dt = parse_data_type parser in
  Diagnostic.EmitDiagnostic
    ( id
      |> Printf.sprintf "expected `:=`, found `%s` in constant named `%s`"
           (show_token parser.current_token),
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator ColonEq);
  Decl (Constant { id; expr = parse_expr2 parser; data_type = dt; is_pub })

and parse_multiple parser =
  let is_lower =
    match parser.current_token with
    | Identifier s -> String.lowercase_ascii s = s
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "miss identifier, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  let rec loop ?(ids = []) () =
    if
      parser.current_token <> Separator RightParen
      && parser.current_token <> Separator Eof
    then (
      let id =
        match parser.current_token with
        | Identifier s ->
            if is_lower <> (s = String.lowercase_ascii s) then
              Diagnostic.EmitDiagnostic
                ( "you cannot define variable and constant in same pattern",
                  Diagnostic.Error,
                  parser.current_location )
              |> raise
            else s
        | _ ->
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "miss identifier, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> raise
      in
      next_token parser;
      if is_data_type parser ~n:0 then (
        let dt = parse_data_type parser in
        if parser.current_token <> Separator RightParen then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        loop ~ids:((id, Some dt) :: ids) ())
      else (
        if parser.current_token <> Separator RightParen then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        loop ~ids:((id, None) :: ids) ()))
    else (
      if parser.current_token <> Separator RightParen then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `)`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator RightParen);
      next_token parser;
      (ids |> List.rev |> Array.of_list, is_lower))
  in
  loop ()

and parse_multiple_variable parser ~ids ~is_mut =
  let loc = Location.copy_location parser.current_location in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `:=`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator ColonEq);
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `(`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator LeftParen);
  let rec loop ?(i = 0) ?(vars = []) () =
    if
      parser.current_token <> Separator RightParen
      && parser.current_token <> Separator Eof
      && i < Array.length ids
    then (
      let expr = parse_expr2 parser in
      if parser.current_token <> Separator RightParen then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `,`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Comma);
      loop ~i:(i + 1)
        ~vars:
          (Decl
             (Variable
                {
                  id = (match ids.(i) with id, _ -> id);
                  expr;
                  data_type = (match ids.(i) with _, dt -> dt);
                  is_mut;
                })
          :: vars)
        ())
    else (
      if matches parser (Separator RightParen) |> Bool.not then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `)`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator RightParen);
      if List.length vars = 0 then
        (Location.end_location loc parser.current_location;
         loc
         |> new_diagnostic Diagnostic.Warning
              "please define a simple variable like this: `<id> := <value>`")
        |> Diagnostic.emit_diagnostic;
      vars |> List.rev |> Array.of_list)
  in
  loop ()

and parse_multiple_constant parser ~ids ~is_pub =
  let loc = Location.copy_location parser.current_location in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `:=`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator ColonEq);
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `(`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator LeftParen);
  let rec loop ?(i = 0) ?(consts = []) () =
    if
      parser.current_token <> Separator RightParen
      && parser.current_token <> Separator Eof
      && i < Array.length ids
    then (
      let expr = parse_expr2 parser in
      if parser.current_token <> Separator RightParen then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `,`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Comma);
      loop ~i:(i + 1)
        ~consts:
          (Decl
             (Constant
                {
                  id = (match ids.(i) with id, _ -> id);
                  expr;
                  data_type =
                    (match ids.(i) with
                    | _, Some dt -> dt
                    | _, None ->
                        Diagnostic.EmitDiagnostic
                          ( parser.current_token |> show_token
                            |> Printf.sprintf
                                 "expected data_type, found `%s`",
                            Diagnostic.Error,
                            parser.current_location )
                        |> raise);
                  is_pub;
                })
          :: consts)
        ())
    else (
      if matches parser (Separator RightParen) |> Bool.not then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `)`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator RightParen);
      if List.length consts = 0 then (
        Location.end_location loc parser.current_location;
        loc
        |> new_diagnostic Diagnostic.Warning
             "please define a simple variable like this: `<id> := <value>`"
        |> Diagnostic.emit_diagnostic);
      consts |> List.rev |> Array.of_list)
  in
  loop ()

and parse_function parser ~is_pub ~is_async ~is_test ~is_export =
  let id =
    match parser.current_token with
    | Identifier s when String.lowercase_ascii s = s -> s
    | Identifier s ->
        Diagnostic.EmitDiagnostic
          ( s |> String.lowercase_ascii
            |> Printf.sprintf
                 "invalid function name `%s`\n\
                  help: define your function name in camel case format, \
                  like this: `%s`"
                 s,
            Diagnostic.Error,
            parser.current_location )
        |> raise
    | _ ->
        Diagnostic.EmitDiagnostic
          ("miss function name", Diagnostic.Error, parser.current_location)
        |> raise
  in
  next_token parser;
  if Separator LeftHook |> matches parser then
    let poly_args = parse_polymorphic_argument parser in
    let args = parse_argument parser in
    if parser.current_token <> Operator Eq then (
      let ret_t = parse_data_type parser in
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf "expected `=`, found `%s` in function name `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Operator Eq);
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Fun
        {
          id;
          poly_args;
          args;
          return_type = Some ret_t;
          body;
          is_async;
          is_export;
          is_pub;
          is_test;
        })
    else (
      next_token parser;
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Fun
        {
          id;
          poly_args;
          args;
          return_type = None;
          body;
          is_async;
          is_export;
          is_pub;
          is_test;
        })
  else
    let args = parse_argument parser in
    if parser.current_token <> Operator Eq then (
      let ret_t = parse_data_type parser in
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf "expected `=`, found `%s` in function name `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Operator Eq);
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Fun
        {
          id;
          poly_args = [||];
          args;
          return_type = Some ret_t;
          body;
          is_async;
          is_export;
          is_pub;
          is_test;
        })
    else (
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf "expected `=`, found `%s` in function name `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Operator Eq);
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Fun
        {
          id;
          poly_args = [||];
          args;
          return_type = None;
          body;
          is_async;
          is_export;
          is_pub;
          is_test;
        })

and parse_body_module parser =
  let rec loop ?(body = []) () =
    if parser.current_token <> Keyword End then
      let loc = Location.copy_location parser.current_location in
      match parse_decl parser with
      | Decl d ->
          Location.end_location loc parser.current_location;
          loop ~body:((Decl d, loc) :: body) ()
      | _ -> failwith "unreachable"
    else (
      next_token parser;
      body |> List.rev |> Array.of_list)
  in
  loop ()

and parse_module parser ~is_pub ~is_test =
  let id =
    match parser.current_token with
    | Identifier s when String.lowercase_ascii s = s -> s
    | Identifier s ->
        Diagnostic.EmitDiagnostic
          ( s
            |> Printf.sprintf
                 "invalid module name `%s`\n\
                  help: define your module name in lower case format like \
                  this `%s`"
                 (String.lowercase_ascii s),
            Diagnostic.Error,
            parser.current_location )
        |> raise
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "miss module name, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `=`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator Eq);
  let body = parse_body_module parser in
  Module { id; body; is_pub; is_test }

and parse_type parser ~is_pub =
  let id =
    match parser.current_token with
    | Identifier s when Char.uppercase_ascii s.[0] = s.[0] -> s
    | Identifier s ->
        Diagnostic.EmitDiagnostic
          ( (Char.uppercase_ascii s.[0] |> String.make 1)
            ^ (String.length s - 1 |> String.sub s 1)
            |> Printf.sprintf
                 "invalid type name: `%s`\n\
                  help define your type name like this: `%s`" s,
            Diagnostic.Error,
            parser.current_location )
        |> raise
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "miss type name, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  let poly_args = ref [] in
  if parser.current_token = Separator LeftHook then (
    next_token parser;
    poly_args :=
      !poly_args @ (parse_polymorphic_argument parser |> Array.to_list));
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `:`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator Colon);
  next_token parser;
  let kw = parser.previous_token in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `=`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator Eq);
  match kw with
  | Keyword Alias ->
      !poly_args |> List.rev |> Array.of_list
      |> parse_alias parser id ~is_pub
  | Keyword Record ->
      !poly_args |> List.rev |> Array.of_list
      |> parse_record parser id ~is_pub
  | Keyword Enum ->
      !poly_args |> List.rev |> Array.of_list |> parse_enum parser id ~is_pub
  | _ ->
      Diagnostic.EmitDiagnostic
        ( "the usage of `type` keyword is not allowed without `alias`, \
           `record`, `enum` definition",
          Diagnostic.Error,
          parser.previous_location )
      |> raise

and parse_alias parser id poly_args ~is_pub =
  let dt = parse_data_type parser in
  Alias { id; poly_args; data_type = dt; is_pub }

and parse_record parser id poly_args ~is_pub =
  let rec loop ?(fields = []) () =
    if parser.current_token <> Keyword End then (
      let loc = Location.copy_location parser.current_location in
      let is_field_pub = ref false in
      if parser.current_token = Keyword Pub then (
        is_field_pub := true;
        next_token parser);
      let id =
        match parser.current_token with
        | Identifier s when String.lowercase_ascii s = s -> s
        | Identifier s ->
            Diagnostic.EmitDiagnostic
              ( s |> String.lowercase_ascii
                |> Printf.sprintf
                     "invalid field name `%s` in record named `%s`\n\
                      help: define your field name in lowercase format like \
                      this: `%s`"
                     s id,
                Diagnostic.Error,
                parser.current_location )
            |> raise
        | _ ->
            raise
              (Diagnostic.EmitDiagnostic
                 ( parser.current_token |> show_token
                   |> Printf.sprintf "miss field name found `%s`",
                   Diagnostic.Error,
                   parser.current_location ))
      in
      next_token parser;
      let dt = parse_data_type parser in
      Location.end_location loc parser.current_location;
      loop
        ~fields:
          (({ id; data_type = dt; is_pub = !is_field_pub }, loc) :: fields)
        ())
    else (
      next_token parser;
      Record
        {
          id;
          poly_args;
          fields = fields |> List.rev |> Array.of_list;
          is_pub;
        })
  in
  loop ()

and parse_enum parser id poly_args ~is_pub =
  let rec loop ?(variants = []) () =
    if parser.current_token <> Keyword End then (
      let loc = Location.copy_location parser.current_location in
      let id =
        match parser.current_token with
        | Identifier s when Char.uppercase_ascii s.[0] = s.[0] -> s
        | Identifier s ->
            Diagnostic.EmitDiagnostic
              ( (Char.uppercase_ascii s.[0] |> String.make 1)
                ^ (String.length s - 1 |> String.sub s 1)
                |> Printf.sprintf
                     "invalid variant name: `%s` in enum named `%s`\n\
                      help define your variant name in camel case format, \
                      like this: `%s`"
                     s id,
                Diagnostic.Error,
                parser.current_location )
            |> raise
        | _ ->
            raise
              (Diagnostic.EmitDiagnostic
                 ( parser.current_token |> show_token
                   |> Printf.sprintf "miss variant name, found `%s`",
                   Diagnostic.Error,
                   parser.current_location ))
      in
      next_token parser;
      if parser.current_token <> Keyword End then (
        if matches parser (Separator Bar) then (
          Location.end_location loc parser.current_location;
          loop ~variants:(({ id; data_type = None }, loc) :: variants) ())
        else
          let dt = parse_data_type parser in
          if parser.current_token <> Keyword End then
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "expected `|`, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> expect_token parser (Separator Bar);
          Location.end_location loc parser.current_location;
          loop ~variants:(({ id; data_type = Some dt }, loc) :: variants) ())
      else (
        Location.end_location loc parser.current_location;
        loop ~variants:(({ id; data_type = None }, loc) :: variants) ()))
    else (
      next_token parser;
      Enum
        {
          id;
          poly_args;
          variants = variants |> List.rev |> Array.of_list;
          is_pub;
        })
  in
  loop ()

and parse_error parser ~is_pub =
  let id =
    match parser.current_token with
    | Identifier s when Char.uppercase_ascii s.[0] = s.[0] -> s
    | Identifier s ->
        Diagnostic.EmitDiagnostic
          ( (Char.uppercase_ascii s.[0] |> String.make 1)
            ^ (String.length s - 1 |> String.sub s 1)
            |> Printf.sprintf
                 "invalid type name: `%s`\n\
                  help define your type name like this: `%s`" s,
            Diagnostic.Error,
            parser.current_location )
        |> raise
    | t ->
        Diagnostic.EmitDiagnostic
          ( t |> show_token |> Printf.sprintf "miss error name, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  let poly_args =
    if parser.current_token = Separator LeftHook then
      parse_polymorphic_argument parser
    else [||]
  in
  let loc = Location.copy_location parser.current_location in
  let dt =
    if is_data_type parser ~n:0 then Some (parse_data_type parser) else None
  in
  Location.end_location loc parser.current_location;
  Error { id; poly_args; variant = (dt, loc); is_pub }

(* object A: <class, trait> = <body> end *)
and parse_object parser ~is_pub =
  let id =
    match parser.current_token with
    | Identifier s when Char.uppercase_ascii s.[0] = s.[0] -> s
    | Identifier s ->
        Diagnostic.EmitDiagnostic
          ( (Char.uppercase_ascii s.[0] |> String.make 1)
            ^ (String.length s - 1 |> String.sub s 1)
            |> Printf.sprintf
                 "invalid object name: `%s`\n\
                  help define your object name like this: `%s`" s,
            Diagnostic.Error,
            parser.current_location )
        |> raise
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "miss object name, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  let poly_args = ref [] in
  let inh = ref [] in
  if parser.current_token = Separator LeftHook then (
    next_token parser;
    poly_args :=
      !poly_args @ (parser |> parse_polymorphic_argument |> Array.to_list));
  if parser.current_token = Separator FatArrow then (
    next_token parser;
    Diagnostic.EmitDiagnostic
      ( parser.current_token |> show_token
        |> Printf.sprintf "expected `[`, found `%s`",
        Diagnostic.Error,
        parser.current_location )
    |> expect_token parser (Separator LeftHook);
    let rec parse_inh ?(l = []) () =
      if parser.current_token <> Separator RightHook then (
        let id =
          match parse_primary_expr parser with
          | Identifier (s, _) -> Identifier (s, None)
          | IdentifierAccess (arr, _) -> IdentifierAccess (arr, None)
          | _ ->
              Diagnostic.EmitDiagnostic
                ( parser.current_token |> show_token
                  |> Printf.sprintf "expected identifier, found `%s`",
                  Diagnostic.Error,
                  parser.current_location )
              |> raise
        in
        next_token parser;
        if parser.current_token <> Separator RightHook then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        parse_inh ~l:(id :: l) ())
      else (
        next_token parser;
        inh := l)
    in
    parse_inh ());
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `:`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator Colon);
  next_token parser;
  let kw = parser.previous_token in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `=`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Operator Eq);
  match kw with
  | Keyword Class ->
      !inh |> List.rev |> Array.of_list
      |> parse_class parser id
           (!poly_args |> List.rev |> Array.of_list)
           ~is_pub
  | Keyword Trait ->
      !poly_args |> List.rev |> Array.of_list
      |> parse_trait parser id ~is_pub
  | _ ->
      Diagnostic.EmitDiagnostic
        ( "the usage of `object` keyword is not allowed without `class`, \
           `trait` definition",
          Diagnostic.Error,
          parser.previous_location )
      |> raise

and parse_property parser ~is_pub =
  let id =
    match parser.current_token with
    | Identifier s -> s
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected identifier, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  if is_data_type parser ~n:0 && parser.current_token <> Keyword Self then
    let dt = parse_data_type parser in
    Property (id, dt, is_pub)
  else parse_method parser id ~is_pub

and parse_body_class parser id =
  let rec loop ?(body = []) () =
    if parser.current_token <> Keyword End then (
      let loc = Location.copy_location parser.current_location in
      let decl =
        match parser.current_token with
        | Separator At ->
            next_token parser;
            Decl (parse_property parser ~is_pub:false)
        | Keyword Pub ->
            next_token parser;
            next_token parser;
            Decl (parse_property parser ~is_pub:true)
        | _ ->
            Location.end_location loc parser.current_location;
            Diagnostic.EmitDiagnostic
              ( id
                |> Printf.sprintf
                     "unexpected expression `%s`, in class named `%s`"
                     (show_token parser.current_token),
                Diagnostic.Error,
                loc )
            |> raise
      in
      Location.end_location loc parser.current_location;
      loop ~body:((decl, loc) :: body) ())
    else (
      next_token parser;
      body |> List.rev |> Array.of_list)
  in
  loop ()

and parse_class parser id poly_args inh ~is_pub =
  let body = parse_body_class parser id in
  Class { id; poly_args; inh; body; is_pub }

and parse_method parser id ~is_pub =
  if parser.current_token = Separator LeftHook then
    let poly_args = parse_polymorphic_argument parser in
    let args = parse_method_argument parser in
    if parser.current_token <> Separator FatArrow then (
      let ret_t = parse_data_type parser in
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf
               "expected `=>`, found `%s` in method named in `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator FatArrow);
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Method { id; poly_args; args; return_type = Some ret_t; body; is_pub })
    else (
      next_token parser;
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Method { id; poly_args; args; return_type = None; body; is_pub })
  else
    let args = parse_method_argument parser in
    if parser.current_token <> Separator FatArrow then (
      let ret_t = parse_data_type parser in
      Diagnostic.EmitDiagnostic
        ( id
          |> Printf.sprintf
               "expected `=>`, found `%s` in method named in `%s`"
               (show_token parser.current_token),
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator FatArrow);
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Method
        {
          id;
          poly_args = [||];
          args;
          return_type = Some ret_t;
          body;
          is_pub;
        })
    else (
      next_token parser;
      let body =
        parse_body parser ~closure:(Some (Keyword End), None, None)
      in
      Method { id; poly_args = [||]; args; return_type = None; body; is_pub })

and parse_trait parser id poly_args ~is_pub =
  let rec loop ?(body = []) () =
    if parser.current_token <> Keyword End then (
      let loc = Location.copy_location parser.current_location in
      let decl =
        match parse_decl parser with
        | Decl (Method { id; poly_args; args; return_type; body; is_pub }) ->
            Decl (Method { id; poly_args; args; return_type; body; is_pub })
        | _ ->
            Diagnostic.EmitDiagnostic
              ( id
                |> Printf.sprintf
                     "this current declaration is not allowed in trait \
                      named `%s`",
                (* TODO: convert decl in string *)
                Diagnostic.Error,
                parser.current_location )
            |> raise
      in
      Location.end_location loc parser.current_location;
      loop ~body:((decl, loc) :: body) ())
    else (
      next_token parser;
      Trait
        { id; poly_args; body = body |> List.rev |> Array.of_list; is_pub })
  in
  loop ()

and parse_import parser ~is_pub =
  let value =
    match parser.current_token with
    | Literal (String s) -> s
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf
                 "you have not define a value of import, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  next_token parser;
  if parser.current_token = Keyword As then (
    next_token parser;
    let as_id =
      match parser.current_token with
      | Identifier s -> s
      | _ ->
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf
                   "expected identifier of `as` keyword but it found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> raise
    in
    next_token parser;
    Import { import = value; is_pub; _as = Some as_id })
  else Import { import = value; is_pub; _as = None }

and parse_pub_block parser =
  let rec loop ?(body = []) () =
    if parser.current_token <> Keyword End then
      let loc = Location.copy_location parser.current_location in
      loop
        ~body:
          ((match parser.current_token with
           | Keyword Fun ->
               next_token parser;
               let f =
                 parse_function parser ~is_pub:true ~is_async:false
                   ~is_test:false ~is_export:false
               in
               Location.end_location loc parser.current_location;
               (f, loc)
           | Keyword Async when peek_token parser ~n:1 = Some (Keyword Fun)
             ->
               next_token parser;
               next_token parser;
               let f =
                 parse_function parser ~is_pub:true ~is_async:true
                   ~is_test:false ~is_export:false
               in
               Location.end_location loc parser.current_location;
               (f, loc)
           | Keyword Async ->
               Diagnostic.EmitDiagnostic
                 ( "the usage of `async` keyword is not allowed without \
                    function definition",
                   Diagnostic.Error,
                   parser.current_location )
               |> raise
           | Identifier s when String.uppercase_ascii s = s -> (
               next_token parser;
               match parse_constant parser ~id:s ~is_pub:true with
               | Decl (Constant { id; data_type; expr; is_pub }) ->
                   Location.end_location loc parser.current_location;
                   (Constant { id; data_type; expr; is_pub }, loc)
               | _ ->
                   Diagnostic.EmitDiagnostic
                     ( "the usage of `pub` keyword is not allowed",
                       Diagnostic.Error,
                       parser.current_location )
                   |> raise)
           | Keyword Module ->
               next_token parser;
               let m = parse_module parser ~is_pub:true ~is_test:false in
               Location.end_location loc parser.current_location;
               (m, loc)
           | Keyword Object ->
               next_token parser;
               let o = parse_object parser ~is_pub:true in
               Location.end_location loc parser.current_location;
               (o, loc)
           | Keyword Type ->
               next_token parser;
               let t = parse_type parser ~is_pub:true in
               Location.end_location loc parser.current_location;
               (t, loc)
           | Keyword Import ->
               next_token parser;
               let i = parse_import parser ~is_pub:true in
               Location.end_location loc parser.current_location;
               (i, loc)
           | Operator Eq ->
               Diagnostic.EmitDiagnostic
                 ( "you cannot define pub block in pub block is not make \
                    sense",
                   Diagnostic.Error,
                   parser.current_location )
               |> raise
           | _ ->
               Diagnostic.EmitDiagnostic
                 ( parser.current_token |> show_token
                   |> Printf.sprintf
                        "unexpected expression in pub block: `%s`",
                   Diagnostic.Error,
                   parser.current_location )
               |> raise)
          :: body)
        ()
    else (
      next_token parser;
      body |> List.rev |> Array.of_list)
  in
  let pub_body = loop () in
  Array.length pub_body - 1
  |> Array.sub pub_body 0
  |> Array.iter (fun (n, loc) ->
         parser.nodes <- Array.append parser.nodes [| (Decl n, loc) |]);
  pub_body.(Array.length pub_body - 1)

and parse_pub parser =
  next_token parser;
  match parser.previous_token with
  | Keyword Fun ->
      parse_function parser ~is_pub:true ~is_async:false ~is_test:false
        ~is_export:false
  | Keyword Async when parser.current_token = Keyword Fun ->
      next_token parser;
      parse_function parser ~is_pub:true ~is_async:true ~is_test:false
        ~is_export:false
  | Keyword Async ->
      Diagnostic.EmitDiagnostic
        ( "the usage of `async` keyword is not allowed without function \
           definition",
          Diagnostic.Error,
          parser.current_location )
      |> raise
  | Identifier s when String.uppercase_ascii s = s -> (
      match parse_constant parser ~id:s ~is_pub:true with
      | Decl (Constant { id; data_type; expr; is_pub }) ->
          Constant { id; data_type; expr; is_pub }
      | _ ->
          Diagnostic.EmitDiagnostic
            ( "the usage of `pub` keyword is not allowed",
              Diagnostic.Error,
              parser.current_location )
          |> raise)
  | Keyword Module -> parse_module parser ~is_pub:true ~is_test:false
  | Keyword Object -> parse_object parser ~is_pub:true
  | Keyword Type -> parse_type parser ~is_pub:true
  | Keyword Import -> parse_import parser ~is_pub:true
  | Operator Eq -> (
      match parse_pub_block parser with
      | n, l ->
          parser.current_location <- l;
          n)
  | Keyword Error -> parse_error parser ~is_pub:true
  | _ ->
      Diagnostic.EmitDiagnostic
        ( "the usage of `pub` keyword is not allowed",
          Diagnostic.Error,
          parser.current_location )
      |> raise

and parse_body parser ~closure =
  let clos1, clos2, clos3 = closure in
  let rec loop ?(body = []) () =
    if
      Some parser.current_token <> clos1
      && Some parser.current_token <> clos2
      && Some parser.current_token <> clos3
    then (
      let loc = Location.copy_location parser.current_location in
      let child =
        match parser.current_token with
        | Identifier _ when is_binop parser ~n:1 -> Expr (parse_expr2 parser)
        | Identifier s when String.uppercase_ascii s = s ->
            next_token parser;
            parse_constant parser ~id:s ~is_pub:false
        | Identifier s
          when String.lowercase_ascii s = s
               && peek_token parser ~n:1 <> Some (Separator LeftParen) ->
            next_token parser;
            parse_variable parser ~id:s ~is_mut:false
        | Keyword Self when peek_token parser ~n:1 = Some (Separator Dot) ->
            Expr (parse_expr2 parser)
        | Keyword Self ->
            next_token parser;
            Expr Self
        | Keyword If ->
            next_token parser;
            Stmt (parse_if parser)
        | Keyword Match ->
            next_token parser;
            Stmt (parse_match parser)
        | Keyword Return ->
            next_token parser;
            Stmt (parse_return parser)
        | Keyword For ->
            next_token parser;
            Stmt (parse_for parser)
        | Keyword While ->
            next_token parser;
            Stmt (parse_while parser)
        | Keyword Await ->
            next_token parser;
            Stmt (parse_await parser)
        | Separator LeftParen
          when peek_token parser ~n:1 <> Some (Separator RightParen) -> (
            next_token parser;
            match parse_grouping parser ~is_mut:false with
            | Decl (Variable { id; data_type; expr; is_mut }) ->
                Decl (Variable { id; data_type; expr; is_mut })
            | _ ->
                Location.end_location loc parser.current_location;
                Diagnostic.EmitDiagnostic
                  ( "unexpected declaration in this scope",
                    Diagnostic.Error,
                    loc )
                |> raise)
        | Keyword Mut ->
            next_token parser;
            let id =
              match parser.current_token with
              | Identifier s when String.lowercase_ascii s = s -> s
              | Identifier s ->
                  Diagnostic.EmitDiagnostic
                    ( s |> String.lowercase_ascii
                      |> Printf.sprintf
                           "invalid variable name `%s`\n\
                            help: define your variable name in camel case \
                            format like this: `%s`"
                           s,
                      Diagnostic.Error,
                      parser.current_location )
                  |> raise
              | _ ->
                  Diagnostic.EmitDiagnostic
                    ( "miss variable identifier",
                      Diagnostic.Error,
                      parser.current_location )
                  |> raise
            in
            next_token parser;
            parse_variable parser ~id ~is_mut:true
        | Keyword Fun ->
            next_token parser;
            Decl
              (parse_function parser ~is_pub:false ~is_async:false
                 ~is_test:false ~is_export:false)
        | Keyword Async when peek_token parser ~n:1 = Some (Keyword Fun) ->
            next_token parser;
            next_token parser;
            Decl
              (parse_function parser ~is_pub:false ~is_async:true
                 ~is_test:false ~is_export:false)
        | Keyword Async ->
            Diagnostic.EmitDiagnostic
              ( "the usage of `async` keyword is not allowed without \
                 function definition",
                Diagnostic.Error,
                parser.current_location )
            |> raise
        | _ -> Expr (parse_expr2 parser)
      in
      Location.end_location loc parser.current_location;
      (* TODO: Review this part of code *)
      if
        Some parser.current_token <> clos1
        && Some parser.current_token <> clos2
        && Some parser.current_token <> clos3
      then
        if
          parser.previous_location.line = parser.current_location.line
          |> Bool.not
        then ()
        else
          Diagnostic.EmitDiagnostic
            ( "unexpected expression",
              Diagnostic.Error,
              parser.current_location )
          |> raise
      else ();
      loop ~body:((child, loc) :: body) ())
    else (
      next_token parser;
      body |> List.rev |> Array.of_list)
  in
  loop ()

and parse_polymorphic_argument parser =
  let rec loop ?(args = []) () =
    if parser.current_token <> Separator RightHook then (
      let p =
        match parse_data_type parser with
        | `Generics s ->
            if parser.current_token = Operator Eq then (
              next_token parser;
              let restricted_type = parse_data_type parser in
              RestrictedDatatype (`Generics s, restricted_type))
            else Datatype (`Generics s)
        | _ ->
            Diagnostic.EmitDiagnostic
              ( "polymorphic parameter expect only parameter data type like \
                 `a`",
                Diagnostic.Error,
                parser.current_location )
            |> raise
      in
      if parser.current_token <> Separator RightHook then
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `,`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Comma);
      loop ~args:(p :: args) ())
    else (
      next_token parser;
      args |> List.rev |> Array.of_list)
  in
  loop ()

and parse_argument parser =
  if
    parser.current_token = Separator LeftParen
    && peek_token parser ~n:1 = Some (Separator RightParen)
  then (
    next_token parser;
    next_token parser;
    [||])
  else if parser.current_token = Separator LeftParen then (
    next_token parser;
    let args = ref [] in
    let rec loop () =
      if parser.current_token <> Separator RightParen then (
        let loc = Location.copy_location parser.current_location in
        let id =
          match parser.current_token with
          | Identifier s when String.lowercase_ascii s = s -> s
          | Identifier s ->
              Diagnostic.emit_diagnostic
                (parser.current_location
                |> new_diagnostic Diagnostic.Error
                     (s
                     |> Printf.sprintf
                          "use lowercase format for argument named `%s`"));
              s
          | _ ->
              Diagnostic.EmitDiagnostic
                ( parser.current_token |> show_token
                  |> Printf.sprintf "miss parameter name, found `%s`",
                  Diagnostic.Error,
                  parser.current_location )
              |> raise
        in
        next_token parser;
        if is_data_type parser ~n:0 then
          let dt = parse_data_type parser in
          if matches parser (Operator Eq) then (
            Location.end_location loc parser.current_location;
            args :=
              !args
              @ [
                  {
                    id;
                    kind = Default (Expr (parse_expr2 parser));
                    data_type = Some dt;
                    loc;
                  };
                ])
          else (
            Location.end_location loc parser.current_location;
            args :=
              !args @ [ { id; kind = Normal; data_type = Some dt; loc } ])
        else if matches parser (Operator Eq) then (
          Location.end_location loc parser.current_location;
          args :=
            !args
            @ [
                {
                  id;
                  kind = Default (Expr (parse_expr2 parser));
                  data_type = None;
                  loc;
                };
              ])
        else (
          Location.end_location loc parser.current_location;
          args := !args @ [ { id; kind = Normal; data_type = None; loc } ]);
        if parser.current_token <> Separator RightParen then
          Diagnostic.EmitDiagnostic
            ( parser.current_token |> show_token
              |> Printf.sprintf "expected `,`, found `%s`",
              Diagnostic.Error,
              parser.current_location )
          |> expect_token parser (Separator Comma);
        loop ())
    in
    loop ();
    next_token parser;
    !args |> List.rev |> Array.of_list)
  else [||]

and parse_method_argument parser =
  if parser.current_token = Separator LeftParen then (
    next_token parser;
    Diagnostic.EmitDiagnostic
      ( "self argument may the first argument in method",
        Diagnostic.Error,
        parser.current_location )
    |> expect_token parser (Keyword Self);
    if parser.current_token = Separator RightParen then (
      next_token parser;
      [|
        { id_mth = None; kind_mth = Normal; data_type_mth = Some `SelfArg };
      |])
    else (
      Diagnostic.EmitDiagnostic
        ( parser.current_token |> show_token
          |> Printf.sprintf "expected `,`, found `%s`",
          Diagnostic.Error,
          parser.current_location )
      |> expect_token parser (Separator Comma);
      let args = ref [] in
      let rec loop () =
        if parser.current_token <> Separator RightParen then (
          let id =
            match parser.current_token with
            | Identifier s when String.lowercase_ascii s = s -> s
            | Identifier s ->
                Diagnostic.emit_diagnostic
                  (parser.current_location
                  |> new_diagnostic Diagnostic.Error
                       (s
                       |> Printf.sprintf
                            "use lowercase format for argument named `%s`"));
                s
            | _ ->
                Diagnostic.EmitDiagnostic
                  ( parser.current_token |> show_token
                    |> Printf.sprintf "miss parameter name, found `%s`",
                    Diagnostic.Error,
                    parser.current_location )
                |> raise
          in
          next_token parser;
          if is_data_type parser ~n:0 then
            let dt = parse_data_type parser in
            if matches parser (Operator Eq) then
              args :=
                !args
                @ [
                    {
                      id_mth = Some id;
                      kind_mth = Default (Expr (parse_expr2 parser));
                      data_type_mth = Some dt;
                    };
                  ]
            else
              args :=
                !args
                @ [
                    {
                      id_mth = Some id;
                      kind_mth = Normal;
                      data_type_mth = Some dt;
                    };
                  ]
          else if matches parser (Operator Eq) then
            args :=
              !args
              @ [
                  {
                    id_mth = Some id;
                    kind_mth = Default (Expr (parse_expr2 parser));
                    data_type_mth = None;
                  };
                ]
          else
            args :=
              !args
              @ [
                  {
                    id_mth = Some id;
                    kind_mth = Normal;
                    data_type_mth = None;
                  };
                ];
          if parser.current_token <> Separator RightParen then
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "expected `,`, found `%s`",
                Diagnostic.Error,
                parser.current_location )
            |> expect_token parser (Separator Comma);
          loop ())
        else next_token parser
      in
      loop ();
      !args |> List.rev |> Array.of_list))
  else (
    Diagnostic.EmitDiagnostic
      ( parser.current_token |> show_token
        |> Printf.sprintf "expected `self` parameter, found `%s`",
        Diagnostic.Error,
        parser.current_location )
    |> expect_token parser (Keyword Self);
    [| { id_mth = None; kind_mth = Normal; data_type_mth = Some `SelfArg } |])

and parse_decl parser =
  let loc = Location.copy_location parser.current_location in
  next_token parser;
  let node =
    match parser.previous_token with
    | Identifier s when String.uppercase_ascii s = s ->
        parse_constant parser ~id:s ~is_pub:false
    | Separator LeftParen -> (
        match parse_grouping parser ~is_mut:false with
        | Decl (Constant { id; data_type; expr; is_pub }) ->
            Decl (Constant { id; data_type; expr; is_pub })
        | _ ->
            Location.end_location loc parser.current_location;
            Diagnostic.EmitDiagnostic
              ("unexpected declaration in this scope", Diagnostic.Error, loc)
            |> raise)
    | Keyword Fun ->
        Decl
          (parse_function parser ~is_pub:false ~is_async:false ~is_test:false
             ~is_export:false)
    | Keyword Async when parser.current_token = Keyword Fun ->
        Decl
          (parse_function parser ~is_pub:false ~is_async:true ~is_test:false
             ~is_export:false)
    | Keyword Async ->
        Diagnostic.EmitDiagnostic
          ( "the usage of `async` keyword is not allowed without function \
             definition",
            Diagnostic.Error,
            parser.previous_location )
        |> raise
    | Keyword Pub -> Decl (parse_pub parser)
    | Keyword Module ->
        Decl (parse_module parser ~is_pub:false ~is_test:false)
    | Keyword Type -> Decl (parse_type parser ~is_pub:false)
    | Keyword Object -> Decl (parse_object parser ~is_pub:false)
    | Keyword Import -> Decl (parse_import parser ~is_pub:false)
    | Keyword Error -> Decl (parse_error parser ~is_pub:false)
    | t -> (
        match t with
        | Keyword End ->
            Diagnostic.EmitDiagnostic
              ( "unexpected end of block",
                Diagnostic.Error,
                parser.current_location )
            |> raise
        | _ ->
            Diagnostic.EmitDiagnostic
              ( parser.current_token |> show_token
                |> Printf.sprintf "unexpected expression `%s`",
                Diagnostic.Error,
                parser.previous_location )
            |> raise)
  in
  Location.end_location loc parser.current_location;
  node

and parse_if parser =
  let cond = parse_expr2 parser in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `do` keyword, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Keyword Do);
  let if_ =
    parse_body parser
      ~closure:(Some (Keyword Elif), Some (Keyword Else), Some (Keyword End))
  in
  if parser.previous_token = Keyword End then
    If { if_ = (cond, if_); elif_ = None; else_ = None }
  else if parser.previous_token = Keyword Elif then
    let rec parse_elif ?(elif_ = []) () =
      if
        parser.current_token = Keyword Elif
        || parser.previous_token = Keyword Elif
      then (
        let cond = parse_expr2 parser in
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `do` keyword, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Keyword Do);
        parse_elif
          ~elif_:
            (( cond,
               parse_body parser
                 ~closure:
                   ( Some (Keyword Elif),
                     Some (Keyword Else),
                     Some (Keyword End) ) )
            :: elif_)
          ())
      else if parser.current_token = Keyword End then
        If
          {
            if_ = (cond, if_);
            elif_ = Some (elif_ |> List.rev |> Array.of_list);
            else_ = None;
          }
      else
        let else_ =
          parse_body parser ~closure:(Some (Keyword End), None, None)
        in
        If
          {
            if_ = (cond, if_);
            elif_ = Some (elif_ |> List.rev |> Array.of_list);
            else_ = Some else_;
          }
    in
    parse_elif ()
  else
    let else_ =
      parse_body parser ~closure:(Some (Keyword End), None, None)
    in
    If { if_ = (cond, if_); elif_ = None; else_ = Some else_ }

and parse_match parser =
  let expr = Expr (parse_expr2 parser) in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `do` keyword, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Keyword Do);
  let rec loop ?(case = []) () =
    match parser.current_token with
    | Keyword End ->
        next_token parser;
        Match { expr; case = case |> List.rev |> Array.of_list }
    | _ ->
        let rec loop_expr ?(expr = [ parse_expr2 parser ]) () =
          if parser.current_token = Separator Bar then (
            next_token parser;
            let e = parse_expr2 parser in
            loop_expr ~expr:(e :: expr) ())
          else expr |> List.rev |> Array.of_list
        in
        let arr_expr = loop_expr () in
        let cond =
          if parser.current_token = Operator Interrogation then (
            next_token parser;
            Some (parse_expr2 parser))
          else None
        in
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected `->`, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> expect_token parser (Separator Arrow);
        let body =
          parse_body parser
            ~closure:(Some (Keyword End), Some (Separator Bar), None)
        in
        let case_ref = ref case in
        let rec loop_case ?(i = 0) () =
          if i < Array.length arr_expr then (
            case_ref := [ { expr = arr_expr.(i); cond; body } ] @ !case_ref;
            loop_case ~i:(i + 1) ())
        in
        loop_case ();
        if
          parser.previous_token = Separator Bar
          && parser.current_token = Keyword End
        then (
          next_token parser;
          Match { expr; case = !case_ref |> List.rev |> Array.of_list })
        else if parser.current_token = Keyword End then
          Match { expr; case = !case_ref |> List.rev |> Array.of_list }
        else loop ~case:!case_ref ()
  in
  loop ()

and parse_try parser =
  let try_body =
    parse_body parser ~closure:(Some (Keyword Catch), None, None)
  in
  let catch_expr = parse_expr2 parser in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `<-`, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Separator InverseArrow);
  let catch_body =
    parse_body parser ~closure:(Some (Keyword End), None, None)
  in
  Try { try_body; catch_expr; catch_body }

and parse_return parser = Return (parse_expr2 parser)
and parse_await parser = Await (parse_expr2 parser)

and parse_while parser =
  let cond = parse_expr2 parser in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `do` keyword, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Keyword Do);
  let body = parse_body parser ~closure:(Some (Keyword End), None, None) in
  While { cond; body }

and parse_for parser =
  let id =
    match parser.current_token with
    | Identifier s ->
        next_token parser;
        Identifier (s, None)
    | Separator LeftParen ->
        next_token parser;
        parse_tuple parser
    | _ ->
        Diagnostic.EmitDiagnostic
          ( parser.current_token |> show_token
            |> Printf.sprintf "expected identifier, found `%s`",
            Diagnostic.Error,
            parser.current_location )
        |> raise
  in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `in` keyword, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Keyword In);
  let expr = parse_expr2 parser in
  Diagnostic.EmitDiagnostic
    ( parser.current_token |> show_token
      |> Printf.sprintf "expected `do` keyword, found `%s`",
      Diagnostic.Error,
      parser.current_location )
  |> expect_token parser (Keyword Do);
  let body = parse_body parser ~closure:(Some (Keyword End), None, None) in
  For { expr = In (id, expr); body }
