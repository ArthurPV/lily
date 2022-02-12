open Lily_parser.Ast
open Lily_lexer.Location

module IsInt : sig
  val is8 : Stdint.int64 -> bool
  val is16 : Stdint.int64 -> bool
  val is32 : Stdint.int64 -> bool
  val is64 : Stdint.int64 -> bool
end

module IsUint : sig
  val is8 : Stdint.int64 -> bool
  val is16 : Stdint.int64 -> bool
  val is32 : Stdint.int64 -> bool
  val is64 : Stdint.int64 -> bool
end

val infer_function_type : ast * location -> call:ast * location -> ast
val infer_variable_type : ast * location -> call:ast * location -> ast
val infer_method_type : ast * location -> call:ast * location -> ast
val infer_record_type : ast * location -> call:ast * location -> ast
val infer_expr_type : ast * location -> ast
val infer_type : ast * location -> ast
