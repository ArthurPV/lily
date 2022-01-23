open Lily_common.Common
open Lily_lexer.Location
open Lily_parser.Ast
module Diagnostic = Lily_lexer.Diagnostic
module Parser = Lily_parser.Parser

type from_access =
  [ `Fun
  | `Constant
  | `Module
  | `Alias
  | `Record
  | `Enum
  | `Variant
  | `Class
  | `Trait
  | `None ]

type scope_access =
  [ `Fun of from_access * string * argument array * location * ast option
  | (* function access => function call *)
    `Identifier of
    from_access * string * location * ast option
  | (* identifier => (variable access, constant access module access, type
       access) *)
    `Type of
    from_access * string * data_type array * location * ast option
  | (* type access => alias, record, enum, class *)
    `Variant of
    from_access * string array * location * variant
  | (* variant access => variant call *)
    `IdentifierAddr of scope_access array ]
(* identifier addr => identifier access *)

type scope = {
  parser : Parser.parser;
  mutable global : scope_access array;
  mutable global_pub : scope_access array;
  mutable used : scope_access array;
      (* for emit warning for all pattern unused *)
}

let new_scope parser =
  Parser.run parser;
  { parser; global = [||]; global_pub = [||]; used = [||] }

[@@@warning "-27"]

(* IMPROVE: rename local rec function *)
let rec get_global_access scope nodes ~p_pub =
  let rec loop ?(access = []) ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with t, _ -> t with
      | Decl (Fun { id; args; body; is_pub; _ }) ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Fun
                   ( `Fun,
                     id,
                     args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Fun
                   ( `Fun,
                     id,
                     args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
      | Decl (Constant { id; is_pub; _ }) ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Identifier
                   ( `Constant,
                     id,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Identifier
                   ( `Constant,
                     id,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
      | Decl (Module { id = id_m; body; is_pub; _ }) ->
          let access_ref = ref access in
          let rec loop_module ?(j = 0) () =
            if j < Array.length body then
              match match body.(j) with n, _ -> n with
              | Decl (Fun { id = id_f; args; body; is_pub; _ }) ->
                  if is_pub then (
                    access_ref :=
                      !access_ref
                      @ [
                          `IdentifierAddr
                            [|
                              `Identifier
                                ( `Module,
                                  id_m,
                                  (match nodes.(i) with _, l -> l),
                                  Some (match nodes.(i) with n, _ -> n) );
                              `Fun
                                ( `Fun,
                                  id_f,
                                  args,
                                  (match body.(j) with _, l -> l),
                                  Some (match body.(j) with n, _ -> n) );
                            |];
                        ];
                    loop_module ~j:(j + 1) ())
                  else loop_module ~j:(j + 1) ()
              | Decl (Module { id = id_m2; body = body_m; is_pub; _ }) ->
                  if is_pub then (
                    let m = get_global_access scope body_m ~p_pub in
                    let rec loop_module2 ?(k = 0) () =
                      if j < Array.length m then (
                        let m_access = ref [ m.(k) ] in
                        m_access :=
                          [
                            `Identifier
                              ( `Module,
                                id_m,
                                (match nodes.(i) with _, l -> l),
                                Some (match nodes.(i) with n, _ -> n) );
                          ]
                          @ !m_access;
                        m_access :=
                          !m_access
                          |> list_insert
                               (`Identifier
                                 ( `Module,
                                   id_m2,
                                   (match body.(j) with _, l -> l),
                                   Some (match body.(j) with n, _ -> n) ))
                               1;
                        access_ref :=
                          !access_ref
                          @ [ `IdentifierAddr (!m_access |> Array.of_list) ];
                        loop_module2 ~k:(k + 1) ())
                    in
                    loop_module2 ();
                    access_ref :=
                      !access_ref
                      @ [
                          `IdentifierAddr
                            [|
                              `Identifier
                                ( `Module,
                                  id_m,
                                  (match nodes.(i) with _, l -> l),
                                  Some (match nodes.(i) with n, _ -> n) );
                              `Identifier
                                ( `Module,
                                  id_m2,
                                  (match body.(j) with _, l -> l),
                                  Some (match body.(j) with n, _ -> n) );
                            |];
                        ];
                    loop_module ~j:(j + 1) ())
                  else loop_module ~j:(j + 1) ()
              | _ -> failwith "unreachable"
          in
          if p_pub && is_pub then (
            loop_module ();
            loop ~access:!access_ref ~i:(i + 1) ())
          else (
            loop_module ();
            loop ~access:!access_ref ~i:(i + 1) ())
      | Decl (Alias { id; poly_args; is_pub; _ }) ->
          if is_pub && p_pub then
            loop
              ~access:
                (`Type
                   ( `Alias,
                     id,
                     poly_args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Type
                   ( `Alias,
                     id,
                     poly_args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ()
      | Decl (Record { id; poly_args; fields; is_pub }) ->
          let rec get_record_access ?(j = 0) ?(lf = fields) () =
            (* TODO: improve search of element in list *)
            if j < Array.length lf then (
              let rec get_record_access2 ?(k = j + 1) () =
                if k < Array.length lf then
                  if
                    (match lf.(i) with f, _ -> f.id)
                    = match lf.(k) with f, _ -> f.id
                  then failwith "error"
                  else get_record_access2 ~k:(k + 1) ()
                else ()
              in
              get_record_access2 ();
              get_record_access ~j:(j + 1) ())
          in
          if p_pub && is_pub then (
            get_record_access ();
            loop
              ~access:
                (`Type
                   ( `Record,
                     id,
                     poly_args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ())
          else (
            get_record_access ();
            loop
              ~access:
                (`Type
                   ( `Record,
                     id,
                     poly_args,
                     (match nodes.(i) with _, l -> l),
                     Some (match nodes.(i) with n, _ -> n) )
                :: access)
              ~i:(i + 1) ())
      | Decl (Enum { id; poly_args; variants; _ }) ->
          (* TODO: review this part of code about public access *)
          let access_ref = ref access in
          let rec iter_enum ?(j = 0) () =
            if j < Array.length variants then
              match match variants.(j) with v, _ -> v with
              | { id = id_f; data_type } ->
                  access_ref :=
                    !access_ref
                    @ [
                        `Variant
                          ( `Enum,
                            [| id; id_f |],
                            (match variants.(j) with _, l -> l),
                            match variants.(j) with v, _ -> v );
                      ];
                  iter_enum ~j:(j + 1) ()
          in
          iter_enum ();
          access_ref :=
            !access_ref
            @ [
                `Type
                  ( `Enum,
                    id,
                    poly_args,
                    (match nodes.(i) with _, l -> l),
                    Some (match nodes.(i) with n, _ -> n) );
              ];
          loop ~access:!access_ref ~i:(i + 1) ()
      | Decl (Pub body_pub) ->
          let pub_block_access =
            get_global_access scope
              (body_pub |> Array.map (fun (d, l) -> (Decl d, l)))
              ~p_pub
          in
          let access_ref = ref access in
          let rec loop_pub ?(j = 0) () =
            if j < Array.length pub_block_access then (
              access_ref := !access_ref @ [ pub_block_access.(j) ];
              loop_pub ~j:(j + 1) ())
          in
          loop_pub ();
          loop ~access:!access_ref ~i:(i + 1) ()
      | Doc _ -> loop ~i:(i + 1) ()
      | _ -> failwith "unreachable"
    else access |> Array.of_list
  in
  loop ()

let verify_if_same_access scope scopes =
  let count_errors = ref 0 in
  let rec loop ?(i = 0) () =
    if i < Array.length scopes then
      match scopes.(i) with
      | `Fun (_, id, _, _, _) ->
          let rec loop_fun ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Fun (_, id2, _, loc, _) when id = id2 ->
                  count_errors := !count_errors + 1;
                  loc
                  |> Parser.new_diagnostic scope.parser Diagnostic.Error
                       (Printf.sprintf
                          "you cannot define the same function name in this \
                           scope: `%s`"
                          id)
                  |> Diagnostic.emit_diagnostic;
                  loop_fun ~j:(j + 1) ()
              | _ -> loop_fun ~j:(j + 1) ()
          in
          loop_fun ();
          loop ~i:(i + 1) ()
      | `Type (_, id, _, _, _) ->
          let rec loop_type ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Type (_, id2, _, loc, _) when id = id2 ->
                  count_errors := !count_errors + 1;
                  loc
                  |> Parser.new_diagnostic scope.parser Diagnostic.Error
                       (Printf.sprintf
                          "you cannot define the same type (record, enum, \
                           alias) name in this scope: `%s`"
                          id2)
                  |> Diagnostic.emit_diagnostic;
                  loop_type ~j:(j + 1) ()
              | _ -> loop_type ~j:(j + 1) ()
          in
          loop_type ();
          loop ~i:(i + 1) ()
      | `Identifier (_, id, _, _) ->
          let rec loop_id ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Identifier (_, id2, loc, _) when id = id2 ->
                  count_errors := !count_errors + 1;
                  loc
                  |> Parser.new_diagnostic scope.parser Diagnostic.Error
                       (Printf.sprintf
                          "you cannot define the same pattern (constant, \
                           module, ...) name in this scope: `%s`"
                          id2)
                  |> Diagnostic.emit_diagnostic;
                  loop_id ~j:(j + 1) ()
              | _ -> loop_id ~j:(j + 1) ()
          in
          loop_id ();
          loop ~i:(i + 1) ()
      | `IdentifierAddr id ->
          let rec loop_id_addr ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `IdentifierAddr id2 when id = id2 ->
                  count_errors := !count_errors + 1;
                  (match id2.(Array.length id2 - 1) with
                  | `Identifier (_, _, loc, _)
                  | `Type (_, _, _, loc, _)
                  | `Fun (_, _, _, loc, _)
                  | `Variant (_, _, loc, _) ->
                      loc
                  | _ -> failwith "unreachable")
                  |> Parser.new_diagnostic scope.parser Diagnostic.Error
                       (Printf.sprintf
                          "you cannot define the same pattern (constant, \
                           module, ...) name in this scope: `%s`"
                          "hello")
                  |> Diagnostic.emit_diagnostic;
                  loop_id_addr ~j:(j + 1) ()
              | _ -> loop_id_addr ~j:(j + 1) ()
          in
          loop_id_addr ();
          loop ~i:(i + 1) ()
      | _ -> loop ~i:(i + 1) ()
  in
  loop ();
  !count_errors

let is_contain_main_fun scope =
  let rec loop ?(i = 0) () =
    if i < Array.length scope.parser.nodes then
      match match scope.parser.nodes.(i) with n, _ -> n with
      | Decl (Fun { id = "main"; _ }) -> (true, i)
      | _ -> loop ~i:(i + 1) ()
    else (false, -1)
  in
  loop ()

let add_ref_on_node expr v =
  (* TODO *)
  match expr with
  | Identifier (s, _) -> Identifier (s, v)
  | _ -> failwith "unreachable"

let rec check_expr scope node loc access =
  (* let result = is_verify_scope_value !node in *)
  let matched = ref false in
  match !node with
  | Expr (Literal _) | Expr Undef | Expr Nil -> !node
  | Expr (Identifier (s, _)) ->
      let rec loop ?(i = 0) () =
        if i < Array.length access && !matched |> Bool.not then (
          let rec loop2 ?(j = 0) () =
            if j < Array.length access.(i) && !matched |> Bool.not then
              if
                Some s
                =
                match access.(i).(j) with
                | `Identifier (_, s2, _, _) -> Some s2
                | _ -> None
              then matched := true (* TODO *)
              else loop2 ~j:(j + 1) ()
          in
          loop2 ();
          loop ~i:(i + 1) ())
      in
      loop ();
      if !matched |> Bool.not then
        loc
        |> Parser.new_diagnostic scope.parser Diagnostic.Error
             (Printf.sprintf "identifier not exists: `%s`" s)
        |> Diagnostic.emit_diagnostic;
      !node
  | Expr (IdentifierAccess (_, _)) | Expr (SelfAccess (_, _)) ->
      failwith "todo"
  | Expr (FunctionCall (_, _)) -> failwith "todo"
  | Expr (RecordCall (_, _)) -> failwith "todo"
  | Expr (ClassCall (_, _)) -> failwith "todo"
  | Expr (AnonymousFunction (_, _)) -> failwith "todo"
  | Expr (Negative l) ->
      Expr
        (Negative (ast_to_expr (check_expr scope (Expr l |> ref) loc access)))
  | Expr (Positive l) ->
      Expr
        (Positive (ast_to_expr (check_expr scope (Expr l |> ref) loc access)))
  | Expr (Not l) ->
      Expr (Not (ast_to_expr (check_expr scope (Expr l |> ref) loc access)))
  | Expr (Grouping l) ->
      Expr
        (Grouping (ast_to_expr (check_expr scope (Expr l |> ref) loc access)))
  | Expr (Add (l, r)) ->
      Expr
        (Add
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Sub (l, r)) ->
      Expr
        (Sub
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Mul (l, r)) ->
      Expr
        (Mul
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Div (l, r)) ->
      Expr
        (Div
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Mod (l, r)) ->
      Expr
        (Mod
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Exp (l, r)) ->
      Expr
        (Exp
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Range (l, r)) ->
      Expr
        (Range
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Lt (l, r)) ->
      Expr
        (Lt
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Gt (l, r)) ->
      Expr
        (Gt
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Le (l, r)) ->
      Expr
        (Le
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Ge (l, r)) ->
      Expr
        (Ge
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (And (l, r)) ->
      Expr
        (And
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Or (l, r)) ->
      Expr
        (Or
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Eq (l, r)) ->
      Expr
        (Eq
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | Expr (Ne (l, r)) ->
      Expr
        (Eq
           ( ast_to_expr (check_expr scope (Expr l |> ref) loc access),
             ast_to_expr (check_expr scope (Expr r |> ref) loc access) ))
  | _ -> failwith "unreachable"

let rec check_fun_scope scope args access nodes =
  (* List all used access in array *)
  (* let used_access_in = ref [||] in *)
  (* Add function parameter in access_in *)
  let rec loop ?(i = 0) ?(access_in = []) () =
    if i < Array.length args then
      match args.(i) with
      | { id; kind; data_type; loc } ->
          loop ~i:(i + 1)
            ~access_in:(`Identifier (`None, id, loc, None) :: access_in)
            ()
    else access_in |> Array.of_list
  in
  let access_in = ref [| loop () |] in
  let rec loop_body ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with t, _ -> t with
      | Decl (Variable { id; expr; _ }) ->
          (* let result = is_verify_scope_value (Expr expr) in if match
             result with b, _ -> b then ( let new_access = ref access in
             new_access := Array.append !new_access !access_in; let rec
             iter_result ?(j = 0) () = if j < Array.length (match result with
             _, r -> r) then ( check_expr (ref (match result with _, r ->
             r.(j))) !new_access; iter_result ~j:(j + 1) ()) in iter_result
             ()); *)
          access_in :=
            Array.append !access_in
              [|
                [|
                  `Identifier
                    ( `None,
                      id,
                      (match nodes.(i) with _, l -> l),
                      Some (match nodes.(i) with n, _ -> n) );
                |];
              |];
          loop_body ~i:(i + 1) ()
      | Stmt (If { if_; elif_; else_ }) ->
          (* IF *)
          (* check_expr (match if_ with e, _ -> e) !access_in; *)
          (* check condition *)
          let access_in_ref = access_in in
          check_fun_scope scope [||] !access_in_ref
            (match if_ with _, b -> b);
          (* ELIF *)
          (match elif_ with
          | Some el ->
              let rec loop_elif ?(i = 0) () =
                if i < Array.length el then (
                  (* check condition *)
                  (* check_expr (match el.(i) with e, _ -> e) !access_in; *)
                  check_fun_scope scope [||] !access_in_ref
                    (match el.(i) with _, b -> b);
                  loop_elif ~i:(i + 1) ())
              in
              loop_elif ()
          | None -> ());
          (* ELSE *)
          check_fun_scope scope [||] !access_in_ref
            (match else_ with Some e -> e | None -> [||]);
          loop_body ~i:(i + 1) ()
      | Stmt (While { cond; body }) -> loop_body ~i:(i + 1) ()
      | Stmt (For { expr; body }) -> loop_body ~i:(i + 1) ()
      | Stmt (Match { expr; case; else_case }) -> loop_body ~i:(i + 1) ()
      | Stmt (Return expr) ->
          (* let result = is_verify_scope_value (Expr expr) in if match
             result with b, _ -> b then ( let rec iter_result ?(j = 0) () =
             if j < Array.length (match result with _, r -> r) then (*
             check_expr (match result with _, r -> r.(j)) !access_in; *)
             iter_result ~j:(j + 1) () in iter_result (); *)
          loop_body ~i:(i + 1) ()
      | _ -> failwith "unreachable"
  in
  loop_body ();
  ()

let check_alias_scope scope nodes = assert false
let check_record_scope scope nodes = assert false
let check_enum_scope scope nodes = assert false
let check_variable_scope scope nodes = assert false
let check_constant_scope scope nodes = assert false
let check_module_scope scope nodes = assert false
let check_block_scope scope nodes = assert false
let check_scope scope nodes = assert false

let run scope =
  if Array.length scope.parser.nodes > 0 then (
    scope.global <- get_global_access scope scope.parser.nodes ~p_pub:false;
    scope.global_pub <-
      get_global_access scope scope.parser.nodes ~p_pub:true;
    if verify_if_same_access scope scope.global > 0 then exit 1;

    let main_fun, idx = is_contain_main_fun scope in
    if main_fun |> Bool.not then (
      (match scope.parser.nodes.(Array.length scope.parser.nodes - 1) with
      | _, l -> l)
      |> Parser.new_diagnostic scope.parser Diagnostic.Internal
           "please add main function.\nhelp: ```fun main = end```"
      |> Diagnostic.emit_diagnostic;
      exit 1);
    ())
  else (
    (match
       scope.parser.lexer.tokens.(Array.length scope.parser.lexer.tokens - 1)
     with
    | _, l -> l)
    |> Parser.new_diagnostic scope.parser Diagnostic.Internal
         "please add main function.\nhelp: ```fun main = end```"
    |> Diagnostic.emit_diagnostic;
    exit 1)

(* Example:

   fun add(x Int8, y Int8) Int8 = x+y (* x -> parameter(0) *) (* y ->
   parameter(1) *) end *)
