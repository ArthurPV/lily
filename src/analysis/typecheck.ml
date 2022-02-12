open Lily_parser.Ast
module Diagnostic = Lily_lexer.Diagnostic
module Parser = Lily_parser.Parser

type typecheck = Parser.parser

[@@@warning "-27"]

let rec can_check_type node = assert false
and check_generics_type tpc node = assert false

and check_binary_expr tpc = function
  | `I8, `I8 -> `I8
  | `I16, `I16 -> `I16
  | `I32, `I32 -> `I32
  | `I64, `I64 -> `I64
  | `U8, `U8 -> `U8
  | `U16, `U16 -> `U16
  | `U32, `U32 -> `U32
  | `U64, `U64 -> `U64
  | `I8, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Int8, but the type on the \
            right has type %s"
      |> failwith
  | `I16, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Int16, but the type on the \
            right has type %s"
      |> failwith
  | `I32, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Int32, but the type on the \
            right type %s"
      |> failwith
  | `I64, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Int64, but the type on the \
            right type %s"
      |> failwith
  | `U8, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left, has type Uint8, but the type on the \
            right type %s"
      |> failwith
  | `U16, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left, has type Uint16, but the type on \
            the right type %s"
      |> failwith
  | `U32, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left, has type Uint32, but the type on \
            the right type %s"
      |> failwith
  | `U64, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left, has type Uint64, but the type on \
            the right type %s"
      |> failwith
  | dt, `I8 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Int8, but the type on the \
            left type %s"
      |> failwith
  | dt, `I16 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Int16, but the type on \
            the left type %s"
      |> failwith
  | dt, `I32 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Int32, but the type on \
            the left type %s"
      |> failwith
  | dt, `I64 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Int64, but the type on \
            the left type %s"
      |> failwith
  | dt, `U8 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Uint8, but the type on \
            the left type %s"
      |> failwith
  | dt, `U16 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Uint16, but the type on \
            the left type %s"
      |> failwith
  | dt, `U32 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Uint32, but the type on \
            the left type %s"
      |> failwith
  | dt, `U64 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type Uint64, but the type on \
            the left type %s"
      |> failwith
  | dt, dt2 ->
      dt |> show_data_type
      |> Printf.sprintf
           "bad type for binary expression: [left: %s, right: %s]"
           (dt2 |> show_data_type)
      |> failwith

and check_expr_type tpc = function
  | Grouping x, l -> check_expr_type tpc (x, l)
  | Positive x, l -> check_expr_type tpc (x, l)
  | Negative x, l -> check_expr_type tpc (x, l)
  | Not x, l -> check_expr_type tpc (x, l)
  | Mul (x, y), l ->
      let left = check_expr_type tpc (x, l) in
      let right = check_expr_type tpc (y, l) in
      check_binary_expr tpc (left, right)
  | Div (x, y), _ -> failwith "not implemented"
  | Mod (x, y), _ -> failwith "not implemented"
  | Add (x, y), _ -> failwith "not implemented"
  | Sub (x, y), _ -> failwith "not implemented"
  | Exp (x, y), _ -> failwith "not implemented"
  | Range (x, y), _ -> failwith "not implemented"
  | Lt (x, y), _ -> failwith "not implemented"
  | Gt (x, y), _ -> failwith "not implemented"
  | Le (x, y), _ -> failwith "not implemented"
  | Ge (x, y), _ -> failwith "not implemented"
  | Eq (x, y), _ -> failwith "not implemented"
  | Ne (x, y), _ -> failwith "not implemented"
  | And (x, y), _ -> failwith "not implemented"
  | Or (x, y), _ -> failwith "not implemented"
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
