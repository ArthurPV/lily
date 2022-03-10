open Lily_parser.Ast

[@@@warning "-27"]

module IsInt = struct
  open Stdint

  let is8 i = i >= Int128.of_int (-128) && i <= Int128.of_int 127
  let is16 i = i >= Int128.of_int (-32768) && i <= Int128.of_int 32767

  let is32 i =
    i >= Int128.of_int (-2147483648) && i <= Int128.of_int 2147483647

  let is64 i = i >= Int128.min_int && i <= Int128.max_int
end

module IsUint = struct
  open Stdint

  let is8 i = i >= Int128.of_int 0 && i <= Int128.of_int 255
  let is16 i = i >= Int128.of_int 0 && i <= Int128.of_int 65535
  let is32 i = i >= Int128.of_int 0 && i <= Int128.of_int 4294967295
  let is64 i = i >= Int128.of_int 0 && i <= Int128.max_int
end

module InferFun = struct
  type t = { dt_of_ret : data_type array; mutable dt : data_type option }

  let infer_return_expr return_expr = assert false
  let infer_function_type return_expr ~call = assert false
  let infer_arg_type args = assert false
end

let infer_integer_type node =
  match node with
  | Expr (Literal (Int32 _)), _ -> `I32
  | Expr (Literal (Int64 _)), _ -> `I64
  | Expr (Literal (Int128 _)), _ -> `I128
  | _ -> failwith "unreachable"

let infer_float_type node = assert false
let infer_tuple_type node = assert false
let infer_array_type node = assert false
let infer_function_type node ~call = assert false
let infer_variable_type node ~call = assert false
let infer_method_type node ~call = assert false
let infer_record_type node ~call = assert false
let infer_expr_type node = assert false
let infer_type node = assert false
