open Lily_parser.Ast
open Lily_lexer.Location

module IsInt : sig
  val is8 : Stdint.int128 -> bool
  val is16 : Stdint.int128 -> bool
  val is32 : Stdint.int128 -> bool
  val is64 : Stdint.int128 -> bool
  val is128 : Stdint.int128 -> bool
end

module IsUint : sig
  val is8 : Stdint.int128 -> bool
  val is16 : Stdint.int128 -> bool
  val is32 : Stdint.int128 -> bool
  val is64 : Stdint.int128 -> bool
  val is128 : Stdint.int128 -> bool
end

module InferFun : sig
  (* dt_of_ret: list of return in function replaced by data type dt: return
     data type specified in function declaration *)
  type t = {
    mutable dt_of_ret : data_type array;
    mutable dt_ret : data_type option;
  }

  val new_t : t

  val get_data_type_from_return_arr :
    t ->
    'a ->
    ('a ->
    specified:data_type option ->
    neg:bool ->
    expr * location ->
    data_type) ->
    (expr * location) array ->
    unit

  val infer_return_expr : t -> data_type

  val infer_function_type :
    (ast * location) array -> call:ast * location -> ast

  val infer_arg_type : argument array -> ast
end

val infer_signed_integer_type :
  ast * location -> specified:data_type option -> data_type

val infer_unsigned_integer_type :
  ast * location -> specified:data_type option -> data_type

val infer_integer_type :
  ast * location -> specified:data_type option -> neg:bool -> data_type

val infer_float_type : location -> specified:data_type option -> data_type

val infer_tuple_type :
  'a ->
  ast * location ->
  ('a ->
  specified:data_type option ->
  neg:bool ->
  expr * location ->
  data_type) ->
  data_type

val infer_array_type : ast * location -> data_type
val infer_function_type : ast * location -> call:ast * location -> ast
val infer_variable_type : ast * location -> call:ast * location -> ast
val infer_method_type : ast * location -> call:ast * location -> ast
val infer_record_type : ast * location -> call:ast * location -> ast
val infer_expr_type : ast * location -> ast
val infer_type : ast * location -> ast
