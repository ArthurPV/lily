open Lily_parser.Ast
open Lily_analysis.Scope

(* Lily IR -> LIR *)
module LIR = struct
  type t =
    | Int of Stdint.int64
        [@printer
          fun fmt i -> fprintf fmt "Int(%s)" (Stdint.Int64.to_string i)]
    | Float of float [@printer fun fmt f -> fprintf fmt "Float(%f)" f]
    | Bool of bool
        [@printer
          fun fmt b -> fprintf fmt "Bool(%s)" (if b then "True" else "False")]
    | String of string [@printer fun fmt s -> fprintf fmt "String(%s)" s]
    | Char of char [@printer fun fmt c -> fprintf fmt "Char(%c)" c]
    | Unit [@printer fun fmt _ -> fprintf fmt "Unit"]
    | Object [@printer fun fmt _ -> fprintf fmt "Object"]
    | Array of t array
        [@printer
          fun fmt arr ->
            let rec loop ?(i = 0) ?(l = []) () =
              if i < Array.length arr then
                loop ~i:(i + 1) ~l:(show arr.(i) :: l) ()
              else String.concat ", " l
            in
            fprintf fmt "Array(%s)" (loop ())]
    | Tuple of t array
    | Fun of string * t array (* TODO: add args *)
    | Variable of string * t
    | Constant of string * t
    | Block of t option * t array
    | Call of t array * t
    | Ret of t [@printer fun fmt v -> fprintf fmt "Ret(%s)" (show v)]
    | Undef [@printer fun fmt _ -> fprintf fmt "Undef"]
    | Nil [@printer fun fmt _ -> fprintf fmt "Nil"]
  [@@deriving show]

  let to_int = function Int i -> i | _ -> failwith "unreachable"
  let to_float = function Float f -> f | _ -> failwith "unreachable"
  let to_bool = function Bool b -> b | _ -> failwith "unreachable"
  let to_string = function String s -> s | _ -> failwith "unreachable"
  let to_char = function Char c -> c | _ -> failwith "unreachable"
end

type compiler = { scope : scope; nodes_value : LIR.t array }

[@@@warning "-27"]

(* EVAL expression *)
let rec compile_expr node =
  match node with
  | Expr (Literal (Int i)) -> LIR.Int i
  | Expr (Literal (Float f)) -> LIR.Float f
  | Expr (Literal (String s)) -> LIR.String s
  | Expr (Literal (Char c)) -> LIR.Char c
  | Expr (Literal (Bool b)) -> LIR.Bool b
  | Expr (Grouping expr) -> compile_expr (Expr expr)
  | Expr (Positive expr) -> compile_expr (Expr expr)
  | Expr (Negative expr) -> compile_expr (Expr expr)
  | Expr (Not expr) -> compile_expr (Expr expr)
  | Expr Undef -> LIR.Undef
  | Expr Nil -> LIR.Nil
  | Decl (Variable { id; data_type; expr; is_mut }) ->
      LIR.Variable (id, compile_expr (Expr expr))
  | Decl (Constant { id; data_type; expr; is_pub }) ->
      LIR.Constant (id, compile_expr (Expr expr))
  | Decl
      (Fun
        {
          id;
          args;
          poly_args;
          return_type;
          body;
          is_pub;
          is_async;
          is_export;
          is_test;
        }) ->
      let body_map = body |> Array.map (fun (t, _) -> t) in
      let rec loop ?(body = []) ?(i = 0) () =
        if i < Array.length body_map then
          loop ~body:(compile_expr body_map.(i) :: body) ~i:(i + 1) ()
        else LIR.Fun (id, body |> Array.of_list)
      in
      loop ()
  | Stmt (Return expr) -> LIR.Ret (compile_expr (Expr expr))
  | _ -> failwith "not implemented"

let compile_decl node = assert false
let compile_fun decl = assert false
let compile_class = assert false

let run compiler =
  let main = compiler.scope.parser.nodes.(compiler.scope.idx_of_main_fun) in
  match match main with n, _ -> n with
  | Decl (Fun { body; _ }) ->
      let rec loop ?(i = 0) () =
        if i < Array.length body then (
          Printf.printf "Hello";
          loop ~i:(i + 1) ())
      in
      loop ()
  | _ -> failwith "unreachable"
