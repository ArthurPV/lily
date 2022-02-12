open Lily_parser.Ast
module Diagnostic = Lily_lexer.Diagnostic
module Parser = Lily_parser.Parser

type typecheck = Parser.parser

[@@@warning "-27"]

let rec can_check_type node = assert false
and check_generics_type tpc node = assert false

and check_arithmetic_expr dts =
  match dts with
  | `I8, `I8 -> `I8
  | `I16, `I16 -> `I16
  | `I32, `I32 -> `I32
  | `I64, `I64 -> `I64
  | `U8, `U8 -> `U8
  | `U16, `U16 -> `U16
  | `U32, `U32 -> `U32
  | `U64, `U64 -> `U64
  | `F32, `F32 -> `F32
  | `F64, `F64 -> `F64
  | `I8, dt
  | `I16, dt
  | `I32, dt
  | `I64, dt
  | `U8, dt
  | `U16, dt
  | `U32, dt
  | `U64, dt
  | `F32, dt
  | `F64, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type %s, but the type on the \
            right has type %s"
           (match dts with t, _ -> t |> show_data_type)
      |> failwith
  | dt, `I8
  | dt, `I16
  | dt, `I32
  | dt, `I64
  | dt, `U8
  | dt, `U16
  | dt, `U32
  | dt, `U64
  | dt, `F32
  | dt, `F64 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type %s, but the type on the \
            left has type %s"
           (match dts with _, t -> t |> show_data_type)
      |> failwith
  | dt, dt2 ->
      dt |> show_data_type
      |> Printf.sprintf
           "bad type for arithmetic expression: [left: %s, right: %s]"
           (dt2 |> show_data_type)
      |> failwith

and check_logical_expr = function
  | `Bool, `Bool -> `Bool
  | `Bool, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Bool, but the type on the \
            right has type %s"
      |> failwith
  | dt, `Bool ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right has type Bool, but the type on the \
            left has type %s"
      |> failwith
  | dt, dt2 ->
      dt |> show_data_type
      |> Printf.sprintf
           "bad type for logical expression: [left: %s, right: %s]"
           (dt2 |> show_data_type)
      |> failwith

and check_expr_type tpc = function
  | Grouping x, l -> check_expr_type tpc (x, l)
  | Positive x, l -> check_expr_type tpc (x, l)
  | Negative x, l -> check_expr_type tpc (x, l)
  | Not x, l -> check_expr_type tpc (x, l)
  | Mul (x, y), l
  | Div (x, y), l
  | Mod (x, y), l
  | Add (x, y), l
  | Sub (x, y), l
  | Exp (x, y), l ->
      let left = check_expr_type tpc (x, l) in
      let right = check_expr_type tpc (y, l) in
      (* TODO: add try *)
      check_arithmetic_expr (left, right)
  | Range (x, y), l -> (
      let left = check_expr_type tpc (x, l) in
      let right = check_expr_type tpc (y, l) in
      match (left, right) with
      | `I8, `I8 -> `I8
      | `I16, `I16 -> `I16
      | `I32, `I32 -> `I32
      | `I64, `I64 -> `I64
      | `U8, `U8 -> `U8
      | `U16, `U16 -> `U16
      | `U32, `U32 -> `U32
      | `U64, `U64 -> `U64
      | `Char, `Char -> `Char
      | `I8, dt
      | `I16, dt
      | `I32, dt
      | `I64, dt
      | `U8, dt
      | `U16, dt
      | `U32, dt
      | `U64, dt ->
          dt |> show_data_type
          |> Printf.sprintf
               "the expression on the right, has type %s, but the type on \
                the left has type %s"
               (left |> show_data_type)
          |> failwith
      | dt, `I8
      | dt, `I16
      | dt, `I32
      | dt, `I64
      | dt, `U8
      | dt, `U16
      | dt, `U32
      | dt, `U64 ->
          dt |> show_data_type
          |> Printf.sprintf
               "the expression on the left, has type %s, but the type on \
                the right has type %s"
               (right |> show_data_type)
          |> failwith
      | dt, dt2 ->
          dt |> show_data_type
          |> Printf.sprintf
               "bad type for range expression: [left: %s, right: %s]"
               (dt2 |> show_data_type)
          |> failwith)
  | Lt (x, y), l
  | Gt (x, y), l
  | Le (x, y), l
  | Ge (x, y), l ->
      let left = check_expr_type tpc (x, l) in
      let right = check_expr_type tpc (y, l) in
      (match (left, right) with
      | `I8, `I8 -> `Bool
      | `I16, `I16 -> `Bool
      | `I32, `I32 -> `Bool
      | `I64, `I64 -> `Bool
      | `U8, `U8 -> `Bool
      | `U16, `U16 -> `Bool
      | `U32, `U32 -> `Bool
      | `U64, `U64 -> `Bool
      | `F32, `F32 -> `Bool
      | `F64, `F64 -> `Bool
      | _ -> failwith "error")
  | Eq (x, y), _ -> failwith "not implemented"
  | Ne (x, y), _ -> failwith "not implemented"
  | And (x, y), l
  | Or (x, y), l ->
      let left = check_expr_type tpc (x, l) in
      let right = check_expr_type tpc (y, l) in
      (match (left, right) with
      | `Bool, `Bool -> `Bool
      | _ -> failwith "error")
  | Assign (x, y), _ -> failwith "not implemented"
  | AddAssign (x, y), _ -> failwith "not implemented"
  | SubAssign (x, y), _ -> failwith "not implemented"
  | MulAssign (x, y), _ -> failwith "not implemented"
  | DivAssign (x, y), _ -> failwith "not implemented"
  | ModAssign (x, y), _ -> failwith "not implemented"
  | ExpAssign (x, y), _ -> failwith "not implemented"
  | FunctionCall (id, args), _ -> failwith "not implemented"
  | ClassCall (id, args), _ -> failwith "not implemented"
  | RecordCall (id, args), _ -> failwith "not implemented"
  | Identifier (x, r), _ -> failwith "not implemented"
  | IdentifierAccess (xs, r), _ -> failwith "not implemented"
  | SelfAccess (xs, r), _ -> failwith "not implemented"
  | AnonymousFunction (args, body), _ -> failwith "not implemented"
  | Tuple vals, _ -> failwith "not implemented"
  | Array vals, _ -> failwith "not implemented"
  | Variant (id, args), _ -> failwith "not implemented"
  | Literal (Int i), _ -> failwith "not implemented"
  | Undef, _ -> failwith "not implemented"
  | Nil, _ -> failwith "not implemented"
  | _ -> failwith "not implemented"

and check_type tpc = function
  | Decl _, _ -> failwith "not implemented"
  | Expr e, l -> check_expr_type tpc (e, l)
  | _ -> failwith "not implemented"
