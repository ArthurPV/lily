open Lily_parser.Ast
open Lily_lexer.Location

module IsInt : sig
  val is8 : Stdint.int128 -> bool
  val is16 : Stdint.int128 -> bool
  val is32 : Stdint.int128 -> bool
  val is64 : Stdint.int128 -> bool
end

module IsUint : sig
  val is8 : Stdint.int128 -> bool
  val is16 : Stdint.int128 -> bool
  val is32 : Stdint.int128 -> bool
  val is64 : Stdint.int128 -> bool
end

module InferFun : sig
  type t = { dt_of_ret : data_type array; mutable dt : data_type option }

  val infer_return_expr : (ast * location) array -> data_type array

  val infer_function_type :
    (ast * location) array -> call:ast * location -> ast

  val infer_arg_type : argument array -> ast
end

val infer_integer_type : ast * location -> data_type
val infer_float_type : ast * location -> data_type
val infer_tuple_type : ast * location -> data_type
val infer_array_type : ast * location -> data_type
val infer_function_type : ast * location -> call:ast * location -> ast
val infer_variable_type : ast * location -> call:ast * location -> ast
val infer_method_type : ast * location -> call:ast * location -> ast
val infer_record_type : ast * location -> call:ast * location -> ast
val infer_expr_type : ast * location -> ast
val infer_type : ast * location -> ast
