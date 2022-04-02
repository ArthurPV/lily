open Infer
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
  | `I128, `I128 -> `I128
  | `U8, `U8 -> `U8
  | `U16, `U16 -> `U16
  | `U32, `U32 -> `U32
  | `U64, `U64 -> `U64
  | `U128, `U128 -> `U128
  | `F32, `F32 -> `F32
  | `F64, `F64 -> `F64
  | `I8, dt
  | `I16, dt
  | `I32, dt
  | `I64, dt
  | `I128, dt
  | `U8, dt
  | `U16, dt
  | `U32, dt
  | `U64, dt
  | `U128, dt
  | `F32, dt
  | `F64, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type `%s`, but the type on the \
            right has type `%s`"
           (match dts with t, _ -> t |> show_data_type)
      |> failwith
  | dt, `I8
  | dt, `I16
  | dt, `I32
  | dt, `I64
  | dt, `I128
  | dt, `U8
  | dt, `U16
  | dt, `U32
  | dt, `U64
  | dt, `U128
  | dt, `F32
  | dt, `F64 ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right, has type `%s`, but the type on the \
            left has type `%s`"
           (match dts with _, t -> t |> show_data_type)
      |> failwith
  | dt, dt2 ->
      dt |> show_data_type
      |> Printf.sprintf
           "bad type for arithmetic expression: [left: `%s`, right: `%s`]"
           (dt2 |> show_data_type)
      |> failwith

and check_logical_expr = function
  | `Bool, `Bool -> `Bool
  | `Bool, dt ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the left has type Bool, but the type on the \
            right has type `%s`"
      |> failwith
  | dt, `Bool ->
      dt |> show_data_type
      |> Printf.sprintf
           "the expression on the right has type Bool, but the type on the \
            left has type `%s`"
      |> failwith
  | dt, dt2 ->
      dt |> show_data_type
      |> Printf.sprintf
           "bad type for logical expression: [left: `%s`, right: `%s`]"
           (dt2 |> show_data_type)
      |> failwith

and check_expr_type tpc ~specified ~neg = function
  | Grouping x, l -> check_expr_type tpc (x, l) ~specified ~neg
  | Positive x, l -> (
      match check_expr_type tpc (x, l) ~specified ~neg with
      | ( `F32 | `F64 | `I8 | `I16 | `I32 | `I64 | `I128 | `U8 | `U16 | `U32
        | `U64 | `U128 ) as t ->
          t
      | _ -> failwith "error")
  | Negative x, l -> (
      match check_expr_type tpc (x, l) ~specified ~neg:true with
      | (`F32 | `F64 | `I8 | `I16 | `I32 | `I64 | `I128) as t -> t
      | _ -> failwith "error")
  | Not x, l -> (
      match check_expr_type tpc (x, l) ~specified ~neg with
      | `Bool -> `Bool
      | _ -> failwith "error")
  | Mul (x, y), l
  | Div (x, y), l
  | Mod (x, y), l
  | Add (x, y), l
  | Sub (x, y), l
  | Exp (x, y), l -> (
      let left = check_expr_type tpc (x, l) ~specified ~neg in
      let right = check_expr_type tpc (y, l) ~specified ~neg in
      try check_arithmetic_expr (left, right)
      with Failure e ->
        Diagnostic.EmitDiagnostic (e, Diagnostic.Error, l) |> raise)
  | Range (x, y), l -> (
      let left = check_expr_type tpc (x, l) ~specified ~neg in
      let right = check_expr_type tpc (y, l) ~specified ~neg in
      match (left, right) with
      | `I8, `I8 -> `I8
      | `I16, `I16 -> `I16
      | `I32, `I32 -> `I32
      | `I64, `I64 -> `I64
      | `I128, `I128 -> `I128
      | `U8, `U8 -> `U8
      | `U16, `U16 -> `U16
      | `U32, `U32 -> `U32
      | `U64, `U64 -> `U64
      | `U128, `U128 -> `U128
      | `Char, `Char -> `Char
      | `I8, dt
      | `I16, dt
      | `I32, dt
      | `I64, dt
      | `I128, dt
      | `U8, dt
      | `U16, dt
      | `U32, dt
      | `U64, dt
      | `U128, dt ->
          dt |> show_data_type
          |> Printf.sprintf
               "the expression on the right, has type `%s`, but the type on \
                the left has type `%s`"
               (left |> show_data_type)
          |> failwith
      | dt, `I8
      | dt, `I16
      | dt, `I32
      | dt, `I64
      | dt, `I128
      | dt, `U8
      | dt, `U16
      | dt, `U32
      | dt, `U64
      | dt, `U128 ->
          dt |> show_data_type
          |> Printf.sprintf
               "the expression on the left, has type `%s`, but the type on \
                the right has type `%s`"
               (right |> show_data_type)
          |> failwith
      | dt, dt2 ->
          dt |> show_data_type
          |> Printf.sprintf
               "bad type for range expression: [left: `%s`, right: `%s`]"
               (dt2 |> show_data_type)
          |> failwith)
  | Lt (x, y), l | Gt (x, y), l | Le (x, y), l | Ge (x, y), l -> (
      let left = check_expr_type tpc (x, l) ~specified ~neg in
      let right = check_expr_type tpc (y, l) ~specified ~neg in
      match (left, right) with
      | `I8, `I8 -> `Bool
      | `I16, `I16 -> `Bool
      | `I32, `I32 -> `Bool
      | `I64, `I64 -> `Bool
      | `I128, `I128 -> `Bool
      | `U8, `U8 -> `Bool
      | `U16, `U16 -> `Bool
      | `U32, `U32 -> `Bool
      | `U64, `U64 -> `Bool
      | `U128, `U128 -> `Bool
      | `F32, `F32 -> `Bool
      | `F64, `F64 -> `Bool
      | `Char, `Char -> `Bool
      | `I8, dt
      | `I16, dt
      | `I32, dt
      | `I64, dt
      | `I128, dt
      | `U8, dt
      | `U16, dt
      | `U32, dt
      | `U64, dt
      | `U128, dt
      | `F32, dt
      | `F64, dt
      | `Char, dt ->
          right |> show_data_type
          |> Printf.sprintf
               "the expression on the right, has type `%s`, but the type on \
                the left has type `%s`"
               (dt |> show_data_type)
          |> failwith
      | dt, `I8
      | dt, `I16
      | dt, `I32
      | dt, `I64
      | dt, `I128
      | dt, `U8
      | dt, `U16
      | dt, `U32
      | dt, `U64
      | dt, `U128
      | dt, `F32
      | dt, `F64
      | dt, `Char ->
          Diagnostic.EmitDiagnostic
            ( left |> show_data_type
              |> Printf.sprintf
                   "the expression on the left, has type `%s`, but the type \
                    on the right has type `%s`"
                   (dt |> show_data_type),
              Diagnostic.Error,
              l )
          |> raise
      | dt, dt2 ->
          Diagnostic.EmitDiagnostic
            ( dt |> show_data_type
              |> Printf.sprintf
                   "bad type for comparison expression: [left: `%s`, right: \
                    `%s`]"
                   (dt2 |> show_data_type),
              Diagnostic.Error,
              l )
          |> raise)
  | Eq (x, y), l | Ne (x, y), l -> (
      let left = check_expr_type tpc (x, l) ~specified ~neg in
      let right = check_expr_type tpc (y, l) ~specified ~neg in
      match (left, right) with
      | dt, dt2 when dt = dt2 -> `Bool
      | dt, dt2 ->
          Diagnostic.EmitDiagnostic
            ( dt2 |> show_data_type
              |> Printf.sprintf
                   "bad type for comparison expression: [left: `%s`, right: \
                    `%s`]"
                   (dt |> show_data_type),
              Diagnostic.Error,
              l )
          |> raise)
  | And (x, y), l | Or (x, y), l -> (
      let left = check_expr_type tpc (x, l) ~specified ~neg in
      let right = check_expr_type tpc (y, l) ~specified ~neg in
      match (left, right) with
      | `Bool, `Bool -> `Bool
      | dt, dt2 ->
          Diagnostic.EmitDiagnostic
            ( dt2 |> show_data_type
              |> Printf.sprintf
                   "bad type for logical expression: [left: `%s`, right: \
                    `%s`]"
                   (dt |> show_data_type),
              Diagnostic.Error,
              l )
          |> raise)
  | Assign (x, y), l
  | AddAssign (x, y), l
  | SubAssign (x, y), l
  | MulAssign (x, y), l
  | DivAssign (x, y), l
  | ModAssign (x, y), l
  | ExpAssign (x, y), l -> (
      let var_dt =
        match x with
        | Identifier (_, r) -> (
            match r with
            | Some (Decl (Variable { data_type; _ })) -> (
                match data_type with
                | Some t -> t
                | None -> failwith "unreachable")
            | _ -> failwith "unreachable")
        | _ -> failwith "unreachable"
      in
      match check_expr_type tpc (y, l) ~specified ~neg with
      | dt when dt = var_dt -> var_dt
      | dt -> failwith "error")
  | FunctionCall (id, _), _ -> (
      match id with
      | Identifier (s, op) -> (
          (* TODO *)
          match op with
          | Some (Decl (Fun { return_type; _ })) -> (
              match return_type with
              | Some t -> t
              | None -> failwith "unreachable")
          | _ -> failwith "unreachable")
      | IdentifierAccess _ -> failwith "todo"
      | _ -> failwith "unreachable")
  | ClassCall (id, args), _ -> failwith "not implemented"
  | RecordCall (id, args), _ -> failwith "not implemented"
  | Identifier (x, r), l -> (
      match r with
      | Some (Decl (Variable { data_type; _ })) -> (
          match data_type with Some t -> t | None -> failwith "unreachable")
      | Some (Decl (Constant { data_type; _ })) -> data_type
      | Some (Expr (Identifier (_, r2))) -> (
          match r2 with
          | Some e -> check_expr_type tpc (ast_to_expr e, l) ~specified ~neg
          | None -> failwith "unreachable")
      | _ -> failwith "unreachable")
  | IdentifierAccess (xs, r), _ -> failwith "not implemented"
  | SelfAccess (xs, r), _ -> failwith "not implemented"
  | AnonymousFunction (args, body), _ -> failwith "not implemented"
  | Tuple vals, _ -> failwith "not implemented"
  | Array vals, _ -> failwith "not implemented"
  | Variant (id, args), _ -> failwith "not implemented"
  | Literal (Int32 i), loc ->
      infer_integer_type (Expr (Literal (Int32 i)), loc) ~specified ~neg
  | Literal (Int64 i), loc ->
      infer_integer_type (Expr (Literal (Int64 i)), loc) ~specified ~neg
  | Literal (Int128 i), loc ->
      infer_integer_type (Expr (Literal (Int128 i)), loc) ~specified ~neg
  | Literal (Float f), loc ->
      infer_float_type loc ~specified
  | Literal (String _), _ -> `String
  | Literal (Char _), _ -> `Char
  | Literal (Bool _), _ -> `Bool
  | Undef, _ -> failwith "not implemented"
  | Nil, _ -> failwith "not implemented"
  | _ -> failwith "not implemented"

and check_type tpc ~specified ~neg = function
  | Decl _, _ -> failwith "not implemented"
  | Expr e, l -> check_expr_type tpc ~specified ~neg (e, l)
  | _ -> failwith "not implemented"

and check_fun_args_type args = assert false
and check_fun_type node = assert false

let check_constant_type tpc access =
  match access with
  | Decl (Constant { id; expr; data_type = dt; _ }), loc ->
      let dt2 =
        check_expr_type tpc (expr, loc) ~specified:(Some dt) ~neg:false
      in
      if dt <> dt2 then
        Diagnostic.EmitDiagnostic
          ( dt |> show_data_type
            |> Printf.sprintf
                 "the inferred `%s` type doesn\'t match the specified `%s` \
                  type"
                 (show_data_type dt2),
            Diagnostic.Error,
            loc )
        |> raise
  | _ -> failwith "unreachable"
