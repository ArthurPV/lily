module Diagnostic = Diagnostic
open Location
open Source
open Token

type lexer = {
  src : source;
  loc : location;
  mutable tokens : (token * location) array;
  mutable errors : Diagnostic.diagnostic array;
}

let new_lexer src = { src; loc = new_location; tokens = [||]; errors = [||] }

let get_keyword id =
  match id with
  | "pub" -> Keyword Pub
  | "self" -> Keyword Self
  | "virtual" -> Keyword Virtual
  | "break" -> Keyword Break
  | "if" -> Keyword If
  | "elif" -> Keyword Elif
  | "else" -> Keyword Else
  | "match" -> Keyword Match
  | "and" -> Keyword And
  | "or" -> Keyword Or
  | "not" -> Keyword Not
  | "while" -> Keyword While
  | "for" -> Keyword For
  | "new" -> Keyword New
  | "undef" -> Keyword Undef
  | "alias" -> Keyword Alias
  | "record" -> Keyword Record
  | "trait" -> Keyword Trait
  | "enum" -> Keyword Enum
  | "fun" -> Keyword Fun
  | "end" -> Keyword End
  | "in" -> Keyword In
  | "import" -> Keyword Import
  | "class" -> Keyword Class
  | "try" -> Keyword Try
  | "catch" -> Keyword Catch
  | "type" -> Keyword Type
  | "object" -> Keyword Object
  | "async" -> Keyword Async
  | "await" -> Keyword Await
  | "module" -> Keyword Module
  | "as" -> Keyword As
  | "do" -> Keyword Do
  | "include" -> Keyword Include
  | "macro" -> Keyword Macro
  | "test" -> Keyword Test
  | "True" -> Keyword True
  | "False" -> Keyword False
  | "return" -> Keyword Return
  | "Char" -> Keyword Char
  | "Int8" -> Keyword Int8
  | "Int16" -> Keyword Int16
  | "Int32" -> Keyword Int32
  | "Int64" -> Keyword Int64
  | "Uint8" -> Keyword Uint8
  | "Uint16" -> Keyword Uint16
  | "Uint32" -> Keyword Uint32
  | "Uint64" -> Keyword Uint64
  | "Float32" -> Keyword Float32
  | "Float64" -> Keyword Float64
  | "String" -> Keyword String
  | "Usize" -> Keyword Usize
  | "Isize" -> Keyword Isize
  | "Bool" -> Keyword Bool
  | "Unit" -> Keyword Unit
  | "nil" -> Keyword Nil
  | "mut" -> Keyword Mut
  | _ -> Identifier id

let next_char lexer =
  if lexer.src.pos < lexer.src.len - 1 then (
    if lexer.src.c = '\n' then (
      lexer.loc.col <- 1;
      lexer.loc.line <- lexer.loc.line + 1)
    else lexer.loc.col <- lexer.loc.col + 1;
    lexer.src.pos <- lexer.src.pos + 1;
    lexer.src.c <- lexer.src.content.[lexer.src.pos])

let previous_char lexer =
  lexer.src.pos <- lexer.src.pos - 1;
  lexer.loc.col <- lexer.loc.col - 1;
  lexer.src.c <- lexer.src.content.[lexer.src.pos]

let start_token lexer =
  lexer.loc.s_line <- lexer.loc.line;
  lexer.loc.s_col <- lexer.loc.col

let end_token lexer =
  lexer.loc.e_line <- lexer.loc.line;
  lexer.loc.e_col <- lexer.loc.col

let peek_char lexer ~n =
  if lexer.src.pos + n < lexer.src.len - 1 then
    Some lexer.src.content.[lexer.src.pos + n]
  else None

let is_digit lexer = lexer.src.c >= '0' && lexer.src.c <= '9'

let is_ident lexer =
  (lexer.src.c >= 'a' && lexer.src.c <= 'z')
  || (lexer.src.c >= 'A' && lexer.src.c <= 'Z')
  || lexer.src.c = '_' || is_digit lexer

let is_hex lexer =
  is_digit lexer
  || (lexer.src.c >= 'a' && lexer.src.c <= 'f')
  || (lexer.src.c >= 'A' && lexer.src.c <= 'F')

let is_oct lexer = lexer.src.c >= '0' && lexer.src.c <= '7'
let is_bin lexer = lexer.src.c >= '0' && lexer.src.c <= '1'

let is_num lexer =
  is_digit lexer
  || (lexer.src.c = '.' && peek_char lexer ~n:1 <> Some '.')
  || lexer.src.c = 'e' || lexer.src.c = 'E'

let new_diagnostic lexer kind msg loc =
  end_token lexer;
  Diagnostic.new_diagnostic ~msg kind loc ~filename:lexer.src.filename

let get_escape lexer ~c =
  match (c, lexer.src.c) with
  | '\\', 'n' ->
      next_char lexer;
      "\\n"
  | '\\', 't' ->
      next_char lexer;
      "\\t"
  | '\\', 'r' ->
      next_char lexer;
      "\\r"
  | '\\', 'c' ->
      next_char lexer;
      "\\c"
  | '\\', _ when lexer.src.len < lexer.src.pos - 1 ->
      Diagnostic.EmitDiagnostic
        ("unclosed string literal", Diagnostic.Error, lexer.loc)
      |> raise
  | '\\', c ->
      Diagnostic.EmitDiagnostic
        ( Printf.sprintf "invalid escape: \\`%c`" c,
          Diagnostic.Error,
          lexer.loc )
      |> raise
  | _ -> String.make 1 c

let rec scan_comment_one lexer =
  if lexer.src.c <> '\n' then (
    next_char lexer;
    scan_comment_one lexer)
  else (
    previous_char lexer;
    Comment One)

let scan_comment_multi lexer =
  next_char lexer;
  next_char lexer;

  let rec loop () =
    if lexer.src.c <> '*' || peek_char lexer ~n:1 <> Some ')' then (
      if lexer.src.pos >= lexer.src.len - 2 then
        Diagnostic.EmitDiagnostic
          ("unclosed comment multi line", Diagnostic.Error, lexer.loc)
        |> raise;
      next_char lexer;
      loop ())
    else (
      next_char lexer;
      next_char lexer;
      Comment Multi)
  in
  loop ()

let scan_comment_doc lexer =
  next_char lexer;
  next_char lexer;
  next_char lexer;
  if lexer.src.c = ' ' then next_char lexer;
  let rec loop ?(doc = []) () =
    if lexer.src.c <> '\n' then (
      next_char lexer;
      loop ~doc:(lexer.src.content.[lexer.src.pos - 1] :: doc) ())
    else (
      previous_char lexer;
      Comment
        (Doc
           (doc |> List.rev
           |> List.map (fun x -> String.make 1 x)
           |> String.concat "")))
  in
  loop ()

let rec scan_identifier ?(id = []) lexer =
  if is_ident lexer then (
    next_char lexer;
    scan_identifier ~id:(lexer.src.content.[lexer.src.pos - 1] :: id) lexer)
  else (
    previous_char lexer;
    get_keyword
      (id |> List.rev
      |> List.map (fun x -> String.make 1 x)
      |> String.concat ""))

let scan_char lexer =
  next_char lexer;
  if lexer.src.c <> '\'' then (
    next_char lexer;
    if lexer.src.c <> '\'' then
      Diagnostic.EmitDiagnostic
        ("unclosed char literal", Diagnostic.Error, lexer.loc)
      |> raise;
    Literal (Char lexer.src.content.[lexer.src.pos - 1]))
  else
    Diagnostic.EmitDiagnostic
      ("invalid char literal", Diagnostic.Error, lexer.loc)
    |> raise

let scan_string lexer =
  next_char lexer;
  let rec loop ?(s = []) () =
    if lexer.src.c <> '\"' then (
      if lexer.src.pos >= lexer.src.len - 2 then
        Diagnostic.EmitDiagnostic
          ("unclosed string literal", Diagnostic.Error, lexer.loc)
        |> raise;
      next_char lexer;
      loop
        ~s:(get_escape lexer ~c:lexer.src.content.[lexer.src.pos - 1] :: s)
        ())
    else Literal (String (s |> List.rev |> String.concat ""))
  in
  loop ()

let scan_hex lexer =
  match (lexer.src.c, peek_char lexer ~n:1) with
  | '0', Some 'x' ->
      next_char lexer;
      next_char lexer;
      let rec loop ?(hex = []) () =
        if is_hex lexer then (
          next_char lexer;
          loop ~hex:(lexer.src.content.[lexer.src.pos - 1] :: hex) ())
        else (
          previous_char lexer;
          match hex with
          | [] ->
              Diagnostic.EmitDiagnostic
                ( "invalid hexadecimal literal: `0x`",
                  Diagnostic.Error,
                  lexer.loc )
              |> raise
          | _ ->
              Literal
                (Int
                   ([ '0'; 'x' ] @ (hex |> List.rev)
                   |> List.map (fun x -> String.make 1 x)
                   |> String.concat "")))
      in
      loop ()
  | _ -> failwith "unreachable"

let scan_oct lexer =
  match (lexer.src.c, peek_char lexer ~n:1) with
  | '0', Some 'o' ->
      next_char lexer;
      next_char lexer;
      let rec loop ?(oct = []) () =
        if is_oct lexer then (
          next_char lexer;
          loop ~oct:(lexer.src.content.[lexer.src.pos - 1] :: oct) ())
        else (
          previous_char lexer;
          match oct with
          | [] ->
              Diagnostic.EmitDiagnostic
                ("invalid octal literal: `0o`", Diagnostic.Error, lexer.loc)
              |> raise
          | _ ->
              Literal
                (Int
                   ([ '0'; 'o' ] @ (oct |> List.rev)
                   |> List.map (fun x -> String.make 1 x)
                   |> String.concat "")))
      in
      loop ()
  | _ -> failwith "unreachable"

let scan_bin lexer =
  match (lexer.src.c, peek_char lexer ~n:1) with
  | '0', Some 'b' ->
      next_char lexer;
      next_char lexer;
      let rec loop ?(bin = []) () =
        if is_bin lexer then (
          next_char lexer;
          loop ~bin:(lexer.src.content.[lexer.src.pos - 1] :: bin) ())
        else (
          previous_char lexer;
          match bin with
          | [] ->
              Diagnostic.EmitDiagnostic
                ("invalid binary literal: `0b`", Diagnostic.Error, lexer.loc)
              |> raise
          | _ ->
              Literal
                (Int
                   ([ '0'; 'b' ] @ (bin |> List.rev)
                   |> List.map (fun x -> String.make 1 x)
                   |> String.concat "")))
      in
      loop ()
  | _ -> failwith "unreachable"

let rec scan_num ?(num = []) ?(is_float = false) lexer =
  if is_num lexer then
    if lexer.src.c = 'e' || lexer.src.c = 'E' then (
      next_char lexer;
      if lexer.src.c = '-' || lexer.src.c = '+' || is_digit lexer then (
        next_char lexer;
        scan_num
          ~num:
            (lexer.src.content.[lexer.src.pos - 1]
            :: lexer.src.content.[lexer.src.pos - 2]
            :: num)
          ~is_float:true lexer)
      else
        Diagnostic.EmitDiagnostic
          ("invalid number literal", Diagnostic.Error, lexer.loc)
        |> raise)
    else if lexer.src.c = '.' && Bool.not is_float then (
      next_char lexer;
      if Bool.not (is_digit lexer) then
        Diagnostic.EmitDiagnostic
          ("invalid number literal", Diagnostic.Error, lexer.loc)
        |> raise
      else
        scan_num
          ~num:(lexer.src.content.[lexer.src.pos - 1] :: num)
          ~is_float:true lexer)
    else if (lexer.src.c = 'e' || lexer.src.c = 'E') && is_float then
      Diagnostic.EmitDiagnostic
        ("invalid number literal", Diagnostic.Error, lexer.loc)
      |> raise
    else if lexer.src.c = '.' && is_float then
      Diagnostic.EmitDiagnostic
        ("invalid number literal", Diagnostic.Error, lexer.loc)
      |> raise
    else (
      next_char lexer;
      scan_num ~num:(lexer.src.content.[lexer.src.pos - 1] :: num) lexer)
  else if is_float then (
    previous_char lexer;
    Literal
      (Float
         (num |> List.rev
         |> List.map (fun x -> String.make 1 x)
         |> String.concat "")))
  else (
    previous_char lexer;
    Literal
      (Int
         (num |> List.rev
         |> List.map (fun x -> String.make 1 x)
         |> String.concat "")))

let get_all_num lexer =
  match (lexer.src.c, peek_char lexer ~n:1) with
  | '0', Some 'x' -> scan_hex lexer
  | '0', Some 'o' -> scan_oct lexer
  | '0', Some 'b' -> scan_bin lexer
  | _, _ -> scan_num lexer

let run lexer =
  if lexer.src.len > 0 then (
    let rec loop () =
      if lexer.src.pos < lexer.src.len - 1 then (
        let rec skip_space lexer =
          if lexer.src.c = ' ' || lexer.src.c = '\t' || lexer.src.c = '\n'
          then (
            next_char lexer;
            skip_space lexer)
        in
        skip_space lexer;

        start_token lexer;
        try
          let tok =
            match
              (lexer.src.c, peek_char lexer ~n:1, peek_char lexer ~n:2)
            with
            | '!', Some '=', _ ->
                next_char lexer;
                Operator BangEq
            | '?', _, _ -> Operator Interrogation
            | '#', _, _ -> Separator Hashtag
            | '~', _, _ -> Separator Wave
            | ',', _, _ -> Separator Comma
            | '&', _, _ -> Operator Ampersand
            | ':', Some ':', _ ->
                next_char lexer;
                Separator ColonColon
            | ':', Some '=', _ ->
                next_char lexer;
                Operator ColonEq
            | ':', _, _ -> Separator Colon
            | '|', _, _ -> Separator Bar
            | '@', _, _ -> Separator At
            | '(', Some '*', _ -> scan_comment_multi lexer
            | '(', _, _ -> Separator LeftParen
            | ')', _, _ -> Separator RightParen
            | '{', _, _ -> Separator LeftBrace
            | '}', _, _ -> Separator RightBrace
            | '[', _, _ -> Separator LeftHook
            | ']', _, _ -> Separator RightHook
            | '+', Some '+', _ ->
                next_char lexer;
                Operator PlusPlus
            | '+', Some '=', _ ->
                next_char lexer;
                Operator PlusEq
            | '+', _, _ -> Operator Plus
            | '-', Some '-', _ ->
                next_char lexer;
                Operator MinusMinus
            | '-', Some '=', _ ->
                next_char lexer;
                Operator MinusEq
            | '-', Some '>', _ ->
                next_char lexer;
                Separator Arrow
            | '-', _, _ -> Operator Minus
            | '*', Some '*', Some '*' -> scan_comment_doc lexer
            | '*', Some '*', _ -> scan_comment_one lexer
            | '*', Some '=', _ ->
                next_char lexer;
                Operator StarEq
            | '*', _, _ -> Operator Star
            | '/', Some '=', _ ->
                next_char lexer;
                Operator SlashEq
            | '/', _, _ -> Operator Slash
            | '%', Some '=', _ ->
                next_char lexer;
                Operator PercentageEq
            | '%', _, _ -> Operator Percentage
            | '^', Some '=', _ ->
                next_char lexer;
                Operator HatEq
            | '^', _, _ -> Operator Hat
            | '=', Some '=', _ ->
                next_char lexer;
                Operator EqEq
            | '=', Some '>', _ ->
                next_char lexer;
                Separator FatArrow
            | '=', _, _ -> Operator Eq
            | '<', Some '=', _ ->
                next_char lexer;
                Operator LeftShiftEq
            | '<', Some '-', _ ->
                next_char lexer;
                Separator InverseArrow
            | '<', _, _ -> Operator LeftShift
            | '>', Some '=', _ ->
                next_char lexer;
                Operator RightShiftEq
            | '>', _, _ -> Operator RightShift
            | '.', Some '.', Some '.' ->
                next_char lexer;
                next_char lexer;
                Separator DotDotDot
            | '.', Some '.', _ ->
                next_char lexer;
                Operator DotDot
            | '.', _, _ -> Separator Dot
            | 'a' .. 'z', _, _ | 'A' .. 'Z', _, _ | '_', _, _ ->
                scan_identifier lexer
            | '0' .. '9', _, _ -> get_all_num lexer
            | '\'', _, _ -> scan_char lexer
            | '\"', _, _ -> scan_string lexer
            | _, _, _ ->
                Diagnostic.EmitDiagnostic
                  ( lexer.src.c |> Printf.sprintf "invalid character: `%c`",
                    Diagnostic.Error,
                    lexer.loc )
                |> raise
          in
          end_token lexer;
          let copy_loc = copy_location lexer.loc in
          lexer.tokens <- [| (tok, copy_loc) |] |> Array.append lexer.tokens;
          next_char lexer;

          if lexer.src.pos = lexer.src.len - 1 then (
            start_token lexer;
            end_token lexer;
            let copy_loc = copy_location lexer.loc in
            lexer.tokens <-
              [| (Separator Eof, copy_loc) |] |> Array.append lexer.tokens)
          else ();
          loop ()
        with Diagnostic.EmitDiagnostic (msg, kind, loc) ->
          let copy_loc = copy_location loc in
          lexer.errors <-
            [| new_diagnostic lexer kind msg copy_loc |]
            |> Array.append lexer.errors;
          next_char lexer;
          loop ())
    in
    loop ();

    let rec show_errors ?(i = 0) () =
      if i < Array.length lexer.errors then (
        Printf.printf "%s" (Diagnostic.diagnostic_to_string lexer.errors.(i));
        show_errors ~i:(i + 1) ())
    in
    show_errors ();

    if Array.length lexer.errors > 0 then exit 1)
  else
    lexer.tokens <-
      [| (Separator Eof, lexer.loc) |] |> Array.append lexer.tokens
