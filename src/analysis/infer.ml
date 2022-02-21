open Lily_parser.Ast

[@@@warning "-27"]

module IsInt = struct
  open Stdint

  let is8 i = i >= Int64.of_int (-128) && i <= Int64.of_int 127
  let is16 i = i >= Int64.of_int (-32768) && i <= Int64.of_int 32767

  let is32 i =
    i >= Int64.of_int (-2147483648) && i <= Int64.of_int 2147483647

  let is64 i = i >= Int64.min_int && i <= Int64.max_int
end

module IsUint = struct
  open Stdint

  let is8 i = i >= Int64.of_int 0 && i <= Int64.of_int 255
  let is16 i = i >= Int64.of_int 0 && i <= Int64.of_int 65535
  let is32 i = i >= Int64.of_int 0 && i <= Int64.of_int 4294967295
  let is64 i = i >= Int64.of_int 0 && i <= Int64.max_int
end

module InferFun = struct
  type t = { dt_of_ret : data_type array; mutable dt : data_type option }

  let infer_arg_type args = assert false
  let infer_function_type node ~call = assert false
end

let infer_function_type node ~call = assert false
let infer_variable_type node ~call = assert false
let infer_method_type node ~call = assert false
let infer_record_type node ~call = assert false
let infer_expr_type node = assert false
let infer_type node = assert false
