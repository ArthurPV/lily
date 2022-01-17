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
  | `None ]

type scope_access =
  [ `Fun of
    from_access * string * argument array * location
    (* function access => function call *)
  | `Identifier of
    from_access * string * location
    (* identifier => (variable access, constant access module access, type
       access) *)
  | `Type of
    from_access * string * data_type array * location
    (* type access => alias, record, enum, class *)
  | `Variant of
    from_access * string array * location
    (* variant access => variant call *)
  | `IdentifierAddr of scope_access array ]
(* identifier addr => identifier access *)

type scope = {
  parser : Parser.parser;
  mutable global : scope_access array;
  mutable global_pub : scope_access array;
}

let new_scope parser =
  Parser.run parser;
  { parser; global = [||]; global_pub = [||] }

[@@@warning "-27"]

(* IMPROVE: rename local rec function *)
let rec get_global_access scope nodes ~p_pub =
  let rec loop ?(access = []) ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with n, _ -> n with
      | Decl
          (Fun
            {
              id;
              poly_args;
              args;
              return_type;
              body;
              is_pub;
              is_async;
              is_test;
              is_export;
            }) ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Fun (`Fun, id, args, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Fun (`Fun, id, args, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ()
      | Decl (Constant { id; data_type; expr; is_pub }) ->
          if p_pub && is_pub then
            loop
              ~access:
                (`Identifier (`Constant, id, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Identifier (`Constant, id, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ()
      | Decl (Module { id = id_m; body; is_pub; is_test }) ->
          let access_ref = ref access in
          let rec loop_module ?(j = 0) () =
            if j < Array.length body then
              match match body.(j) with n, _ -> n with
              | Decl
                  (Fun
                    {
                      id = id_f;
                      poly_args;
                      args;
                      return_type;
                      body;
                      is_pub;
                      is_async;
                      is_test;
                      is_export;
                    }) ->
                  if is_pub then (
                    access_ref :=
                      !access_ref
                      @ [
                          `IdentifierAddr
                            [|
                              `Identifier
                                ( `Module,
                                  id_m,
                                  match nodes.(i) with _, l -> l );
                              `Fun
                                ( `Fun,
                                  id_f,
                                  args,
                                  match body.(j) with _, l -> l );
                            |];
                        ];
                    loop_module ~j:(j + 1) ())
                  else loop_module ~j:(j + 1) ()
              | Decl (Module { id = id_m2; body = body_m; is_pub; is_test })
                ->
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
                                match nodes.(i) with _, l -> l );
                          ]
                          @ !m_access;
                        m_access :=
                          !m_access
                          |> list_insert
                               (`Identifier
                                 ( `Module,
                                   id_m2,
                                   match body.(j) with _, l -> l ))
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
                                  match nodes.(i) with _, l -> l );
                              `Identifier
                                ( `Module,
                                  id_m2,
                                  match body.(j) with _, l -> l );
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
      | Decl (Alias { id; poly_args; data_type; is_pub }) ->
          if is_pub && p_pub then
            loop
              ~access:
                (`Type
                   (`Alias, id, poly_args, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ()
          else
            loop
              ~access:
                (`Type
                   (`Alias, id, poly_args, match nodes.(i) with _, l -> l)
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
                   (`Record, id, poly_args, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ())
          else (
            get_record_access ();
            loop
              ~access:
                (`Type
                   (`Record, id, poly_args, match nodes.(i) with _, l -> l)
                :: access)
              ~i:(i + 1) ())
      | Decl (Enum { id; poly_args; variants; is_pub }) ->
          let access_ref = ref access in
          let rec iter_enum ?(j = 0) () =
            if j < Array.length variants then
              match match variants.(j) with v, _ -> v with
              | { id = id_f; data_type } ->
                  access_ref :=
                    !access_ref
                    @ [
                        `IdentifierAddr
                          [|
                            `Identifier
                              (`Enum, id, match nodes.(i) with _, l -> l);
                            `Identifier
                              ( `Variant,
                                id_f,
                                match variants.(j) with _, l -> l );
                          |];
                      ];
                  iter_enum ~j:(j + 1) ()
          in
          iter_enum ();
          access_ref :=
            !access_ref
            @ [
                `Type (`Enum, id, poly_args, match nodes.(i) with _, l -> l);
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
      | `Fun (_, id, _, _) ->
          let rec loop_fun ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Fun (_, id2, _, loc) when id = id2 ->
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
      | `Type (_, id, _, _) ->
          let rec loop_type ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Type (_, id2, _, loc) when id = id2 ->
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
      | `Identifier (_, id, _) ->
          let rec loop_id ?(j = i + 1) () =
            if j < Array.length scopes then
              match scopes.(j) with
              | `Identifier (_, id2, loc) when id = id2 ->
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
                  | `Identifier (_, _, loc)
                  | `Type (_, _, _, loc)
                  | `Fun (_, _, _, loc)
                  | `Variant (_, _, loc) ->
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

let rec is_verify_scope_value expr =
  match expr with
  | Expr (Literal _) -> (false, [||])
  | Expr (Negative l) | Expr (Positive l) | Expr (Not l) | Expr (Grouping l)
    ->
      let try_l = is_verify_scope_value (Expr l) in
      if match try_l with b, _ -> b then (true, match try_l with _, e -> e)
      else (false, [||])
  | Expr (Add (l, r))
  | Expr (Sub (l, r))
  | Expr (Mul (l, r))
  | Expr (Div (l, r))
  | Expr (Mod (l, r))
  | Expr (Exp (l, r))
  | Expr (Range (l, r))
  | Expr (Lt (l, r))
  | Expr (Gt (l, r))
  | Expr (Le (l, r))
  | Expr (Ge (l, r))
  | Expr (And (l, r))
  | Expr (Or (l, r))
  | Expr (Eq (l, r))
  | Expr (Ne (l, r)) ->
      let try_l = is_verify_scope_value (Expr l) in
      let try_r = is_verify_scope_value (Expr r) in
      if (match try_l with b, _ -> b) || match try_r with b, _ -> b then (
        let to_verify = ref (match try_l with _, e -> e) in
        to_verify := Array.append !to_verify (match try_r with _, e -> e);
        (true, !to_verify))
      else (false, [||])
  | Expr _ -> (true, [| expr |])
  | _ -> failwith "unreachable"

let check_expr scope access =
  (* TODO: modify expr reference *)
  ()

let check_fun_scope scope args access nodes =
  let rec loop ?(i = 0) ?(access_in = []) () =
    if i < Array.length args then
      match args.(i) with
      | { id; kind; data_type; loc } ->
          loop ~i:(i + 1)
            ~access_in:(`Identifier (`None, id, loc) :: access_in)
            ()
    else access_in |> Array.of_list
  in

  let access_in = ref [| loop () |] in
  let rec loop_body ?(i = 0) () =
    if i < Array.length nodes then
      match match nodes.(i) with t, _ -> t with
      | Decl (Variable { id; data_type; expr; is_mut }) ->
          let result = is_verify_scope_value (Expr expr) in
          if match result with b, _ -> b then (
            let new_access = ref access in
            new_access := Array.append !new_access !access_in;
            let rec iter_result ?(j = 0) () =
              if j < Array.length (match result with _, r -> r) then (
                check_expr (match result with _, r -> r.(j)) !new_access;
                iter_result ~j:(j + 1) ())
            in
            iter_result ();
            access_in :=
              Array.append !access_in
                [|
                  [|
                    `Identifier (`None, id, match nodes.(i) with _, l -> l);
                  |];
                |];
            loop_body ~i:(i + 1) ())
      | Stmt (If { if_; elif_; else_ }) -> loop_body ~i:(i + 1) ()
      | Stmt (While { cond; body }) -> loop_body ~i:(i + 1) ()
      | Stmt (For { expr; body }) -> loop_body ~i:(i + 1) ()
      | Stmt (Match { expr; case; else_case }) -> loop_body ~i:(i + 1) ()
      | Stmt (Return expr) ->
          let result = is_verify_scope_value (Expr expr) in
          if match result with b, _ -> b then (
            let rec iter_result ?(j = 0) () =
              if j < Array.length (match result with _, r -> r) then (
                check_expr (match result with _, r -> r.(j)) !access_in;
                iter_result ~j:(j + 1) ())
            in
            iter_result ();
            loop_body ~i:(i + 1) ())
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
  scope.global <- get_global_access scope scope.parser.nodes ~p_pub:false;
  scope.global_pub <- get_global_access scope scope.parser.nodes ~p_pub:true;
  if verify_if_same_access scope scope.global > 0 then exit 1
