open Lily_common.Common
open Lily_lexer.Location
open Lily_parser.Ast
open Buffer
open Typecheck
module Diagnostic = Lily_lexer.Diagnostic
module Parser = Lily_parser.Parser
module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module CliError = Lily_common.Error

type from_access =
  [ `Fun
  | `Constant
  | `Module
  | `Alias
  | `Record
  | `Enum
  | `Variant
  | `Class
  | `Property
  | `Method
  | `Trait
  | `None ]
[@@deriving show]

type scope_access =
  [ `Fun of from_access * string * argument array * location * ast option
    [@printer
      fun fmt f ->
        let args = match f with _, _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length args then
            loop ~i:(i + 1) ~l:(show_argument args.(i))
          else String.concat ", " l
        in
        fprintf fmt "Fun(%s, %s, %s, %s, %s)"
          (match f with acc, _, _, _, _ -> show_from_access acc)
          (match f with _, s, _, _, _ -> s)
          (loop ())
          (match f with _, _, _, loc, _ -> show_location loc)
          (match f with
          | _, _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* function access => function call *)
    `Identifier of
    from_access * string * location * ast option
    [@printer
      fun fmt i ->
        fprintf fmt "Identifier(%s, %s, %s, %s)"
          (match i with acc, _, _, _ -> show_from_access acc)
          (match i with _, s, _, _ -> s)
          (match i with _, _, loc, _ -> show_location loc)
          (match i with
          | _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* identifier => (variable access, constant access module access, type
       access) *)
    `Type of
    from_access * string * data_type array * location * ast option
    [@printer
      fun fmt t ->
        let arr_dt = match t with _, _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length arr_dt then
            loop ~i:(i + 1) ~l:(show_data_type arr_dt.(i) :: l) ()
          else String.concat ", " l
        in
        fprintf fmt "Type(%s, %s, %s, %s, %s)"
          (match t with acc, _, _, _, _ -> show_from_access acc)
          (match t with _, s, _, _, _ -> s)
          (loop ())
          (match t with _, _, _, loc, _ -> show_location loc)
          (match t with
          | _, _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* type access => alias, record, enum, class *)
    `Variant of
    from_access * string array * location * variant
    [@printer
      fun fmt v ->
        let arr = match v with _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length arr then loop ~i:(i + 1) ~l:(arr.(i) :: l)
          else String.concat ", " l
        in
        fprintf fmt "Variant(%s, %s, %s, %s)"
          (match v with acc, _, _, _ -> show_from_access acc)
          (loop ())
          (match v with _, _, loc, _ -> show_location loc)
          (match v with _, _, _, v -> show_variant v)]
  | (* variant access => variant call *)
    `IdentifierAddr of scope_access array
    [@printer
      fun fmt acc ->
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length acc then
            loop ~i:(i + 1) ~l:(show_scope_access acc.(i) :: l) ()
          else String.concat ", " l
        in
        fprintf fmt "IdentifierAddr(%s)" (loop ())] ]
(* identifier addr => identifier access *)
[@@deriving show]

type scope = {
  parser : Parser.parser;
  buffer : scope buffer;
  mutable global : scope_access array;
  mutable global_pub : scope_access array;
  mutable used : scope_access array;
  mutable idx_of_main_fun : int; (* for emit warning for all pattern unused *)
}

let new_scope parser =
  Parser.run parser;
  {
    parser;
    buffer = new_buffer [||];
    global = [||];
    global_pub = [||];
    used = [||];
    idx_of_main_fun = -1;
  }

let push_used scope access =
  scope.used <- Array.append scope.used [| access |]

let rec emit_unused scope = function
  | `Fun (_, id, _, loc, _) ->
      loc
      |> Parser.new_diagnostic scope.parser Diagnostic.Warning
           (id |> Printf.sprintf "unused function `%s`")
      |> Diagnostic.emit_diagnostic
  | `Identifier (from, id, loc, _) ->
      let msg =
        match from with
        | `Constant -> id |> Printf.sprintf "unused constant `%s`"
        | `Module -> id |> Printf.sprintf "unused module `%s`"
        | `Property -> id |> Printf.sprintf "unused property `%s`"
        | `Method -> id |> Printf.sprintf "unused method `%s`"
        | `None -> id |> Printf.sprintf "unused identifier `%s`"
        | _ -> failwith "unreachable"
      in
      loc
      |> Parser.new_diagnostic scope.parser Diagnostic.Warning msg
      |> Diagnostic.emit_diagnostic
  | `Type (from, id, _, loc, _) ->
      let msg =
        match from with
        | `Alias -> id |> Printf.sprintf "unused alias `%s`"
        | `Record -> id |> Printf.sprintf "unused record `%s`"
        | `Enum -> id |> Printf.sprintf "unused enum `%s`"
        | `Class -> id |> Printf.sprintf "unused class `%s`"
        | `Trait -> id |> Printf.sprintf "unused trait `%s`"
        | _ -> failwith "unreachable"
      in
      loc
      |> Parser.new_diagnostic scope.parser Diagnostic.Warning msg
      |> Diagnostic.emit_diagnostic
  | `Variant (_, ids, loc, _) ->
      loc
      |> Parser.new_diagnostic scope.parser Diagnostic.Warning
           (ids.(Array.length ids - 1)
           |> Printf.sprintf "unused variant `%s`")
      |> Diagnostic.emit_diagnostic
  | `IdentifierAddr accs -> emit_unused scope accs.(Array.length accs - 1)

let verify_if_used scope =
  let rec loop ?(i = 0) () =
    if i < Array.length scope.global then
      let matched = ref false in
      (* SKIP MAIN FUNCTION *)
      match scope.global.(i) with
      | `Fun (_, id, _, _, _) when id = "main" -> loop ~i:(i + 1) ()
      | _ ->
          let rec loop2 ?(j = 0) () =
            if j < Array.length scope.used && !matched |> Bool.not then (
              if scope.global.(i) = scope.used.(j) then matched := true
              else ();
              loop2 ~j:(j + 1) ())
          in
          loop2 ();

          if !matched |> Bool.not then emit_unused scope scope.global.(i);
          loop ~i:(i + 1) ()
  in
  loop ()

[@@@warning "-27"]

(* IMPROVE: rename local rec function *)
let rec get_global_access scope nodes ~p_pub =
  let rec loop ?(access = []) ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with t, _ -> t with
      | Decl (Fun { id; args; body; is_pub; _ }) as node_f ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Fun
                   ( `Fun,
                     id,
                     args,
                     (match nodes.(i) with _, l -> l),
                     Some node_f )
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
                     Some node_f )
                :: access)
              ~i:(i + 1) ()
      | Decl (Constant { id; is_pub; _ }) as node_c ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Identifier
                   ( `Constant,
                     id,
                     (match nodes.(i) with _, l -> l),
                     Some node_c )
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Identifier
                   ( `Constant,
                     id,
                     (match nodes.(i) with _, l -> l),
                     Some node_c )
                :: access)
              ~i:(i + 1) ()
      | Decl (Module { id = id_m; body; is_pub; _ }) as node ->
          let access_ref = ref access in
          let rec loop_module ?(j = 0) () =
            if j < Array.length body then
              match match body.(j) with n, _ -> n with
              | Decl (Fun { id = id_f; args; body; is_pub; _ }) as node_f ->
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
                                  Some node );
                              `Fun
                                ( `Fun,
                                  id_f,
                                  args,
                                  (match body.(j) with _, l -> l),
                                  Some node_f );
                            |];
                        ];
                    loop_module ~j:(j + 1) ())
                  else loop_module ~j:(j + 1) ()
              | Decl (Module { id = id_m2; body = body_m; is_pub; _ }) as
                node_m ->
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
                                Some node );
                          ]
                          @ !m_access;
                        m_access :=
                          !m_access
                          |> list_insert
                               (`Identifier
                                 ( `Module,
                                   id_m2,
                                   (match body.(j) with _, l -> l),
                                   Some node_m ))
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
                                  Some node );
                              `Identifier
                                ( `Module,
                                  id_m2,
                                  (match body.(j) with _, l -> l),
                                  Some node_m );
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
      | Decl (Alias { id; poly_args; is_pub; _ }) as node ->
          if is_pub && p_pub then
            loop
              ~access:
                (`Type
                   ( `Alias,
                     id,
                     poly_args,
                     (match nodes.(i) with _, l -> l),
                     Some node )
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
                     Some node )
                :: access)
              ~i:(i + 1) ()
      | Decl (Record { id; poly_args; fields; is_pub }) as node_rec ->
          let rec get_record_access ?(j = 0) ?(lf = fields) () =
            (* TODO: improve search of element in list *)
            if j < Array.length lf then (
              let rec get_record_access2 ?(k = j + 1) () =
                if k < Array.length lf then
                  if
                    (match lf.(i) with f, _ -> f.id)
                    = match lf.(k) with f, _ -> f.id
                  then
                    (match lf.(i) with _, l -> l)
                    |> Parser.new_diagnostic scope.parser Diagnostic.Error
                         (match lf.(i) with
                         | f, _ ->
                             f.id
                             |> Printf.sprintf
                                  "the record `%s` has same fields `%s`" id)
                    |> Diagnostic.emit_diagnostic
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
                     Some node_rec )
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
                     Some node_rec )
                :: access)
              ~i:(i + 1) ())
      | Decl (Enum { id; poly_args; variants; _ }) as node_e ->
          (* TODO: review this part of code about public access *)
          let access_ref = ref access in
          let rec iter_enum ?(j = 0) () =
            if j < Array.length variants then
              match match variants.(j) with v, _ -> v with
              | { id = id_f; data_type } as var ->
                  access_ref :=
                    !access_ref
                    @ [
                        `Variant
                          ( `Enum,
                            [| id; id_f |],
                            (match variants.(j) with _, l -> l),
                            var );
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
                    Some node_e );
              ];
          loop ~access:!access_ref ~i:(i + 1) ()
      | Decl (Class { id; poly_args; is_pub; body; _ }) as node_cl ->
          let access_ref = ref access in
          let rec iter_class ?(j = 0) () =
            if i < Array.length body then (
              (match match body.(j) with n, _ -> n with
              | Decl (Property (id_prop, _, is_pub)) as prop ->
                  if is_pub then
                    access_ref :=
                      !access_ref
                      @ [
                          `IdentifierAddr
                            [|
                              `Identifier
                                ( `Class,
                                  id,
                                  (match nodes.(i) with _, l -> l),
                                  Some node_cl );
                              `Identifier
                                ( `Property,
                                  id_prop,
                                  (match body.(j) with _, l -> l),
                                  Some prop );
                            |];
                        ]
              | Decl (Method { id = id_met; is_pub; _ }) as met ->
                  if is_pub then
                    access_ref :=
                      !access_ref
                      @ [
                          `IdentifierAddr
                            [|
                              `Identifier
                                ( `Class,
                                  id,
                                  (match nodes.(i) with _, l -> l),
                                  Some node_cl );
                              `Identifier
                                ( `Method,
                                  id_met,
                                  (match body.(j) with _, l -> l),
                                  Some met );
                            |];
                        ]
              | _ -> failwith "todo");
              iter_class ~j:(j + 1) ())
          in
          iter_class ();
          loop ~access:!access_ref ~i:(i + 1) ()
      | Decl (Import _) -> loop ~i:(i + 1) ()
      | Doc _ -> loop ~i:(i + 1) ()
      | _ -> failwith "unreachable"
    else access |> Array.of_list
  in
  loop ()

and verify_if_same_access scope scopes =
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

and run_import scope ~path ~as_value ~is_pub loc =
  match Source.read_file path with
  | Ok content -> (
      let scope_buf =
        content |> Source.new_source path |> Lexer.new_lexer
        |> Parser.new_parser |> new_scope
      in
      run scope_buf;
      push_buffer scope.buffer ~filename:path ~content
        (scope_buf.parser.lexer.tokens |> Array.map (fun (x, _) -> x))
        (scope_buf.parser.nodes |> Array.map (fun (x, _) -> x))
        scope_buf;
      match as_value with
      | "" ->
          scope.parser.nodes <-
            Array.append
              (scope_buf.parser.nodes
              |> Parser.change_nodes_visibility ~visibility:is_pub)
              scope.parser.nodes
      | s ->
          scope.parser.nodes <-
            Array.append
              [|
                ( Decl
                    (Module
                       {
                         id = s;
                         body =
                           scope_buf.parser.nodes
                           |> Parser.change_nodes_visibility
                                ~visibility:is_pub;
                         is_pub;
                         is_test = false;
                       }),
                  loc );
              |]
              scope.parser.nodes)
  | Error err -> CliError.print_cli_error (CliError.show_cli_error_kind err)

and resolve_import scope ~value ~as_value ~is_pub loc =
  if value |> String.split_on_char '@' |> List.length >= 1 then
    if value |> String.split_on_char '.' |> List.length >= 1 then
      let value_split =
        List.nth (value |> String.split_on_char '@') 0
        |> String.split_on_char '.'
      in
      let rec loop ?(i = 0) ?(path = "") () =
        if i < List.length value_split then
          loop ~i:(i + 1) ~path:(path ^ List.nth value_split i ^ "/") ()
        else path
      in
      let mod_path =
        List.nth (value |> String.split_on_char '@') 1
        |> String.split_on_char '.'
      in
      let path = loop () ^ List.nth mod_path 0 in
      run_import scope ~path ~as_value ~is_pub loc
    else
      let path = List.nth (value |> String.split_on_char '@') 1 in
      run_import scope ~path ~as_value ~is_pub loc
  else if value |> String.split_on_char '#' |> List.length >= 1 then
    let path =
      match List.nth (value |> String.split_on_char '#') 1 with
      | "std" -> "lib/std/std.lily"
      | _ -> failwith "todo"
    in
    run_import scope ~path ~as_value ~is_pub loc
  else
    loc
    |> Parser.new_diagnostic scope.parser Diagnostic.Error
         (Printf.sprintf
            "bad import value: `%s`\n\
             help: if you want import module use this syntax: ```import \
             \"@<mod_name>\"```\n\
             else if you want import library use this syntax: ```import \
             \"#<lib_name>\"```"
            value)
    |> Diagnostic.emit_diagnostic

and resolve_all_imports scope =
  Array.iter
    (fun x ->
      match x with
      | Decl (Import { import; _as; is_pub }), loc ->
          resolve_import scope ~value:import
            ~as_value:(match _as with None -> "" | Some s -> s)
            ~is_pub loc
      | _ -> ())
    scope.parser.nodes

and is_contain_main_fun scope =
  let rec loop ?(i = 0) () =
    if i < Array.length scope.parser.nodes then
      match match scope.parser.nodes.(i) with n, _ -> n with
      | Decl (Fun { id = "main"; _ }) -> (true, i)
      | _ -> loop ~i:(i + 1) ()
    else (false, -1)
  in
  loop ()

and add_ref_on_node expr v =
  (* TODO *)
  match expr with
  | Identifier (s, _) -> Identifier (s, v)
  | _ -> failwith "unreachable"

(* TODO: for improve code create a context type and pass it in function
   parameter. *)
and check_expr scope node loc access =
  (* let result = is_verify_scope_value !node in *)
  let matched = ref false in
  match !node with
  | Expr (Literal _) | Expr Undef | Expr Nil -> !node
  | Expr (Identifier (s, _)) ->
      let rec loop ?(i = 0) () =
        if i < Array.length access && !matched |> Bool.not then (
          let rec loop2 ?(j = 0) () =
            if j < Array.length access.(i) && !matched |> Bool.not then
              match access.(i).(j) with
              | `Identifier (_, s2, _, ast) when s = s2 ->
                  matched := true;
                  node := Expr (Identifier (s, ast))
              | _ -> loop2 ~j:(j + 1) ()
          in
          loop2 ();
          loop ~i:(i + 1) ())
      in
      loop ();
      if !matched |> Bool.not then
        loc
        |> Parser.new_diagnostic scope.parser Diagnostic.Error
             (Printf.sprintf "cannot find identifier `%s` in this scope" s)
        |> Diagnostic.emit_diagnostic;
      !node
  | Expr (IdentifierAccess (_, _)) | Expr (SelfAccess (_, _)) ->
      failwith "todo"
  | Expr (FunctionCall (e, arr)) -> (
      (* VERIFY EXPR *)
      let rec loop ?(i = 0) () =
        if i < Array.length arr then
          match
            check_expr scope
              (match arr.(i) with _, e_call -> e_call |> ref)
              loc access
          with
          | _ ->
              ();
              loop ~i:(i + 1) ()
      in
      loop ();

      (* SEARCH IF FUNCTION EXISTS *)
      match e with
      | Identifier (s, _) ->
          let rec loop ?(i = 0) () =
            if i < Array.length access && !matched |> Bool.not then (
              let rec loop2 ?(j = 0) () =
                if j < Array.length access.(i) && !matched |> Bool.not then
                  match access.(i).(j) with
                  | `Fun (_, s2, _, _, ast) when s = s2 ->
                      matched := true;
                      push_used scope access.(i).(j);
                      check_fun_scope scope
                        (match ast with
                        | Some (Decl (Fun { args; _ })) -> args
                        | _ -> failwith "unreachable")
                        arr
                        (* (access |> Array.map (fun x -> x |> ref)) *)
                        [| ref scope.global |]
                        (match ast with
                        | Some (Decl (Fun { body; _ })) -> body
                        | _ -> failwith "unreachable")
                        (Some loc);
                      node := Expr (FunctionCall (Identifier (s, ast), arr))
                  | _ -> loop2 ~j:(j + 1) ()
              in
              loop2 ();
              loop ~i:(i + 1) ())
          in
          loop ();
          if !matched |> Bool.not then
            loc
            |> Parser.new_diagnostic scope.parser Diagnostic.Error
                 (Printf.sprintf "cannot find function `%s` in this scope" s)
            |> Diagnostic.emit_diagnostic;
          !node
      | IdentifierAccess (id, _) -> (
          let addr =
            match
              identifier_access_node_to_identifier_addr scope (Expr e) loc
            with
            | `IdentifierAddr arr -> arr
            | _ -> failwith "unreachable"
          in
          match addr.(Array.length addr - 1) with
          | `Fun (_, _, _, loc, ast) ->
              check_fun_scope scope
                (match ast with
                | Some (Decl (Fun { args; _ })) -> args
                | _ -> failwith "unreachable")
                arr [||]
                (match ast with
                | Some (Decl (Fun { body; _ })) -> body
                | _ -> failwith "unreachable")
                (Some loc);
              node := Expr (FunctionCall (IdentifierAccess (id, ast), arr));
              !node
          | _ -> failwith "unreachable")
      | _ -> failwith "unreachable")
  | Expr (RecordCall (e, arr)) -> (
      (* VERIFY EXPR *)
      let rec loop ?(i = 0) () =
        if i < Array.length arr then
          match
            check_expr scope
              (match arr.(i) with
              | _, Some e_call -> Expr e_call |> ref
              | _ -> failwith "todo")
              loc access
          with
          | _ ->
              ();
              loop ~i:(i + 1) ()
      in
      loop ();

      (* SEARCH IF RECORD EXISTS *)
      match e with
      | Identifier (_, _) -> failwith "todo"
      | IdentifierAccess (_, _) -> failwith "todo"
      | _ -> failwith "unreachable")
  | Expr (ClassCall (e, arr)) -> (
      (* VERIFY EXPR *)
      let rec loop ?(i = 0) () =
        if i < Array.length arr then
          match
            check_expr scope
              (match arr.(i) with e_call -> e_call |> ref)
              loc access
          with
          | _ ->
              ();
              loop ~i:(i + 1) ()
      in
      loop ();

      (* SEARCH IF CLASS EXISTS *)
      match e with
      | Identifier (_, _) -> failwith "todo"
      | IdentifierAccess (_, _) -> failwith "todo"
      | _ -> failwith "unreachable")
  | Expr (Array arr) ->
      Expr
        (Array
           (arr
           |> Array.map (fun x ->
                  check_expr scope (Expr x |> ref) loc access |> ast_to_expr)
           ))
  | Expr (Tuple arr) ->
      Expr
        (Tuple
           (arr
           |> Array.map (fun x ->
                  check_expr scope (Expr x |> ref) loc access |> ast_to_expr)
           ))
  | Expr (AnonymousFunction (_, _)) -> failwith "todo"
  | Expr (Negative l) ->
      Expr
        (Negative (check_expr scope (Expr l |> ref) loc access |> ast_to_expr))
  | Expr (Positive l) ->
      Expr
        (Positive (check_expr scope (Expr l |> ref) loc access |> ast_to_expr))
  | Expr (Not l) ->
      Expr (Not (check_expr scope (Expr l |> ref) loc access |> ast_to_expr))
  | Expr (Grouping l) ->
      Expr
        (Grouping (check_expr scope (Expr l |> ref) loc access |> ast_to_expr))
  | Expr (Add (l, r)) ->
      Expr
        (Add
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Sub (l, r)) ->
      Expr
        (Sub
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Mul (l, r)) ->
      Expr
        (Mul
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Div (l, r)) ->
      Expr
        (Div
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Mod (l, r)) ->
      Expr
        (Mod
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Exp (l, r)) ->
      Expr
        (Exp
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Range (l, r)) ->
      Expr
        (Range
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Lt (l, r)) ->
      Expr
        (Lt
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Gt (l, r)) ->
      Expr
        (Gt
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Le (l, r)) ->
      Expr
        (Le
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Ge (l, r)) ->
      Expr
        (Ge
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (And (l, r)) ->
      Expr
        (And
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Or (l, r)) ->
      Expr
        (Or
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Eq (l, r)) ->
      Expr
        (Eq
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | Expr (Ne (l, r)) ->
      Expr
        (Ne
           ( check_expr scope (Expr l |> ref) loc access |> ast_to_expr,
             check_expr scope (Expr r |> ref) loc access |> ast_to_expr ))
  | _ -> failwith "unreachable"

and search_in scope access ~pos ~value loc =
  let rec loop ?(i = 0) ?(v = None) () =
    if i < Array.length access && v = None then
      match access.(i) with
      | `Fun (_, s, _, _, _) -> (
          match value with
          | FunctionCall (id, _) -> (
              match id with
              | Identifier (s2, _) when s = s2 ->
                  loop ~i:(i + 1) ~v:(Some access.(i)) ()
              | Identifier _ -> loop ~i:(i + 1) ()
              | _ -> failwith "unreachable")
          | _ -> loop ~i:(i + 1) ())
      | `Identifier (_, s, _, _) -> (
          match value with
          | Identifier (s2, _) when s = s2 ->
              loop ~i:(i + 1) ~v:(Some access.(i)) ()
          | Identifier _ -> loop ~i:(i + 1) ()
          | _ -> failwith "unreachable")
      | _ -> loop ~i:(i + 1) ()
    else v
  in
  let access_op =
    match value with
    | Identifier (s, _) -> (
        let rec match_value ?(i = 0) ?(a = []) () =
          if i < Array.length access then
            match access.(i) with
            | `IdentifierAddr arr -> (
                match arr.(pos) with
                | `Identifier (_, s2, _, _) when s2 = s ->
                    match_value ~i:(i + 1) ~a:(access.(i) :: a) ()
                | _ -> match_value ~i:(i + 1) ())
            | _ -> match_value ~i:(i + 1) ()
          else a |> List.rev |> Array.of_list
        in
        match match_value () with
        | [||] ->
            loc
            |> Parser.new_diagnostic scope.parser Diagnostic.Error
                 (Printf.sprintf
                    "cannot find this identifier `%s` in identifier access" s)
            |> Diagnostic.emit_diagnostic;
            exit 1
        | v -> Some v)
    | _ -> None
  in
  (loop (), access_op)

and identifier_access_node_to_identifier_addr scope node loc =
  let access = ref scope.global in
  match node with
  | Expr (IdentifierAccess (es, _)) -> (
      let rec loop ?(i = 0) ?(addr = None) () =
        if i < Array.length es && addr = None then
          match es.(i) with
          | FunctionCall (_, _) -> (
              match search_in scope !access ~pos:i ~value:es.(i) loc with
              | Some a, None ->
                  if i + 1 <> Array.length es then failwith "todo"
                  else loop ~i:(i + 1) ~addr:(Some a) ()
              | _ -> failwith "unreachable")
          | RecordCall (_, _) -> (
              match search_in scope !access ~pos:i ~value:es.(i) loc with
              | Some a, None ->
                  if i + 1 <> Array.length es then failwith "todo"
                  else loop ~i:(i + 1) ~addr:(Some a) ()
              | _ -> failwith "unreachable")
          | ClassCall (_, _) -> (
              match search_in scope !access ~pos:i ~value:es.(i) loc with
              | Some a, None -> loop ~i:(i + 1) ~addr:(Some a) ()
              | _ -> failwith "unreachable")
          | Identifier (_, _) -> (
              match search_in scope !access ~pos:i ~value:es.(i) loc with
              | Some a, Some new_access ->
                  access := new_access;
                  loop ~i:(i + 1) ~addr:(Some a) ()
              | _ -> failwith "unreachable")
          | _ -> failwith "unreachable"
        else addr
      in
      match loop () with Some v -> v | None -> failwith "unreachable")
  | _ -> failwith "unreachable"

and push_access_in access_in len access =
  if len + 1 <> Array.length !access_in then
    access_in := Array.append [| ref [| access |] |] !access_in
  else !access_in.(0) <- Array.append !(!access_in.(0)) [| access |] |> ref

and check_duplicate_argument_name scope args =
  let rec loop ?(i = 0) () =
    if i < Array.length args then (
      let rec loop2 ?(j = i + 1) () =
        if j < Array.length args then
          if
            (match args.(i) with { id; _ } -> id)
            = match args.(j) with { id; _ } -> id
          then
            (match args.(i) with { loc; _ } -> loc)
            |> Parser.new_diagnostic scope.parser Diagnostic.Error
                 (Printf.sprintf
                    "the argument have same `%s` name in function definition"
                    (match args.(i) with { id; _ } -> id))
            |> Diagnostic.emit_diagnostic
          else loop2 ~j:(j + 1) ()
      in
      loop2 ();
      loop ~i:(i + 1) ())
  in
  loop ()

and check_count_argument scope args call loc =
  if Array.length args <> Array.length call then
    let n_args = Array.length args in
    let n_call = Array.length call in
    loc
    |> Parser.new_diagnostic scope.parser Diagnostic.Error
         (Printf.sprintf "this function takes %d %s but %d %s was supplied"
            n_args
            (if n_args > 1 then "arguments" else "argument")
            n_call
            (if n_call > 1 then "arguments" else "argument"))
    |> Diagnostic.emit_diagnostic
  else ()

and get_argument_access scope args call =
  let args_without_default_arg =
    args |> Array.to_list
    |> List.filter (fun x ->
           match x with
           | { kind; _ } -> (
               match kind with Default _ -> false | _ -> true))
    |> Array.of_list
  in
  let args_with_just_default_arg =
    args |> Array.to_list
    |> List.filter (fun x ->
           match x with
           | { kind; _ } -> (
               match kind with Default _ -> true | _ -> false))
    |> Array.of_list
  in
  let rec loop ?(i = 0) ?(count_args = 0) ?(access = []) () =
    if i < Array.length call then
      match call.(i) with
      | Some s, v ->
          let rec loc ?(j = 0) () =
            if j < Array.length args_with_just_default_arg then
              match args_with_just_default_arg.(j) with
              | { id; loc; _ } when s = id -> loc
              | _ -> loc ~j:(j + 1) ()
            else (
              (match args.(0) with { loc; _ } -> loc)
              |> Parser.new_diagnostic scope.parser Diagnostic.Error
                   (Printf.sprintf "cannot find this optional argument `%s`"
                      s)
              |> Diagnostic.emit_diagnostic;
              exit 1)
          in
          loop ~i:(i + 1)
            ~access:
              (`Identifier
                 (`None, s, loc (), Some (Expr (Identifier (s, Some v))))
              :: access)
            ()
      | None, v ->
          let id =
            match args_without_default_arg.(i) with { id; _ } -> id
          in
          loop ~i:(i + 1)
            ~access:
              (`Identifier
                 ( `None,
                   id,
                   (match args_without_default_arg.(i) with
                   | { loc; _ } -> loc),
                   Some (Expr (Identifier (id, Some v))) )
              :: access)
            ()
    else access |> Array.of_list
  in
  loop ()

and check_fun_scope scope args call access nodes loc =
  (* List all used access in array *)
  (* let used_access_in = ref [||] in *)
  let return_expr = ref [||] in
  (* Add function parameter in access_in *)
  let len = Array.length access in
  (match loc with
  | Some l ->
      (* SIMPLE CHECK FOR FUN ARGUMENT *)
      check_duplicate_argument_name scope args;
      check_count_argument scope args call l
  (* CHECK TYPE FOR FUN ARGUMENT *)
  | None -> ());
  (* Main function or not in function *)
  let access_in =
    Array.append [| get_argument_access scope args call |> ref |] access
    |> ref
  in
  let rec loop_body ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with t, _ -> t with
      | Decl (Variable { id; data_type; expr; is_mut }) ->
          let checked_expr =
            !access_in
            |> Array.map (fun x -> !x)
            |> check_expr scope (Expr expr |> ref)
                 (match nodes.(i) with _, l -> l)
          in
          nodes.(i) <-
            (match nodes.(i) with
            | _, l ->
                ( Decl
                    (Variable
                       {
                         id;
                         data_type =
                           Some
                             (check_expr_type scope.parser
                                (expr, match nodes.(i) with _, l -> l));
                         expr = ast_to_expr checked_expr;
                         is_mut;
                       }),
                  l ));
          push_access_in access_in len
            (`Identifier
              ( `None,
                id,
                (match nodes.(i) with _, l -> l),
                Some (match nodes.(i) with n, _ -> n) ));
          loop_body ~i:(i + 1) ()
      | Stmt (If { if_; elif_; else_ }) ->
          (* check condition *)
          let checked_if_cond =
            !access_in
            |> Array.map (fun x -> !x)
            |> check_expr scope
                 (Expr (match if_ with e, _ -> e) |> ref)
                 (match nodes.(i) with _, l -> l)
            (* TODO: add location on if expr *)
          in
          let access_in_ref = access_in in
          check_fun_scope scope [||] [||] !access_in_ref
            (match if_ with _, b -> b)
            None;
          (* ELIF *)
          (match elif_ with
          | Some el ->
              let rec loop_elif ?(i = 0) () =
                if i < Array.length el then (
                  (* check condition *)
                  (* check_expr (match el.(i) with e, _ -> e) !access_in; *)
                  check_fun_scope scope [||] [||] !access_in_ref
                    (match el.(i) with _, b -> b)
                    None;
                  loop_elif ~i:(i + 1) ())
              in
              loop_elif ()
          | None -> ());
          (* ELSE *)
          check_fun_scope scope [||] [||] !access_in_ref
            (match else_ with Some e -> e | None -> [||])
            None;
          nodes.(i) <-
            (match nodes.(i) with
            | _, l ->
                ( Stmt
                    (If
                       {
                         if_ =
                           ( ast_to_expr checked_if_cond,
                             match if_ with _, b -> b );
                         elif_;
                         else_;
                       }),
                  l ));
          (* Review this code *)
          loop_body ~i:(i + 1) ()
      | Stmt (While { cond; body }) ->
          (* CHECK condition *)
          let check_while_cond =
            !access_in
            |> Array.map (fun x -> !x)
            |> check_expr scope (Expr cond |> ref)
                 (match nodes.(i) with _, l -> l)
            (* TODO: add location on expression *)
          in
          let access_in_ref = access_in in
          check_fun_scope scope args [||] !access_in_ref body None;
          nodes.(i) <-
            (match nodes.(i) with
            | _, l ->
                ( Stmt (While { cond = ast_to_expr check_while_cond; body }),
                  l ));
          (* Review this code *)
          loop_body ~i:(i + 1) ()
      | Stmt (For { expr; body }) -> loop_body ~i:(i + 1) () (* TODO *)
      | Stmt (Match { expr; case; else_case }) ->
          loop_body ~i:(i + 1) () (* TODO *)
      | Stmt (Return expr) ->
          let check_return_expr =
            !access_in
            |> Array.map (fun x -> !x)
            |> check_expr scope (Expr expr |> ref)
                 (match nodes.(i) with _, l -> l)
          in
          return_expr := Array.append !return_expr [| check_return_expr |];
          (* ADD RETURN FOR CHECK TYPE *)
          nodes.(i) <-
            (match nodes.(i) with
            | _, l -> (Stmt (Return (check_return_expr |> ast_to_expr)), l));
          loop_body ~i:(i + 1) ()
      | Expr expr ->
          let check_expr =
            !access_in
            |> Array.map (fun x -> !x)
            |> check_expr scope (Expr expr |> ref)
                 (match nodes.(i) with _, l -> l)
          in
          (if i + 1 = Array.length nodes then
           return_expr := Array.append !return_expr [| check_expr |]
          else
            match expr with
            | Assign _ | FunctionCall _ ->
                ()
                (* TODO: maybe add warning of function call if return type is
                   not equal to unit type *)
            | _ -> failwith "warning");
          (* ADD RETURN FOR CHECK TYPE *)
          nodes.(i) <- (match nodes.(i) with _, l -> (check_expr, l));
          loop_body ~i:(i + 1) ()
      | _ -> failwith "unreachable"
  in
  loop_body ();
  if verify_if_same_access scope !(!access_in.(0)) > 0 then () else ();
  ()

and run scope =
  if Array.length scope.parser.nodes > 0 then (
    resolve_all_imports scope;
    scope.global <- get_global_access scope scope.parser.nodes ~p_pub:false;
    scope.global_pub <-
      get_global_access scope scope.parser.nodes ~p_pub:true;
    if verify_if_same_access scope scope.global > 0 then exit 1;
    let main_fun, idx = is_contain_main_fun scope in
    if main_fun |> Bool.not then (
      scope.idx_of_main_fun <- idx;
      (match scope.parser.nodes.(Array.length scope.parser.nodes - 1) with
      | _, l -> l)
      |> Parser.new_diagnostic scope.parser Diagnostic.Internal
           "please add main function.\nhelp: ```fun main = end```"
      |> Diagnostic.emit_diagnostic;
      exit 1);
    check_fun_scope scope [||] [||]
      [| ref scope.global |]
      (match scope.parser.nodes.(idx) with
      | n, _ -> (
          match n with
          | Decl (Fun { body; _ }) -> body
          | _ -> failwith "unreachable"))
      None;
    ();
    verify_if_used scope)
  else (
    (match
       scope.parser.lexer.tokens.(Array.length scope.parser.lexer.tokens - 1)
     with
    | _, l -> l)
    |> Parser.new_diagnostic scope.parser Diagnostic.Internal
         "please add main function.\nhelp: ```fun main = end```"
    |> Diagnostic.emit_diagnostic;
    exit 1)

let check_alias_scope scope nodes = assert false
let check_record_scope scope nodes = assert false
let check_enum_scope scope nodes = assert false
let check_variable_scope scope nodes = assert false
let check_constant_scope scope nodes = assert false
let check_module_scope scope nodes = assert false
let check_block_scope scope nodes = assert false
let check_scope scope nodes = assert false

(* Example:

   fun add(x Int8, y Int8) Int8 = x+y (* x -> parameter(0) *) (* y ->
   parameter(1) *) end *)
