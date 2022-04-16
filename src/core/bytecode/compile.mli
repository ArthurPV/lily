open Lily_parser.Ast
open Lily_lexer.Location
open Opcode

val compile_expr :
  dt:data_type option ->
  (expr * location) option ->
  opcode list ->
  opcode list

val compile_integer : dt:data_type option -> expr -> opcode
val compile_float : dt:data_type option -> expr -> opcode
val get_string_id_from_identifier : expr -> string
val can_recude_expression : expr -> bool
val reduce_expression : dt:data_type option -> expr -> opcode
val compile_variable : ast * location -> opcode list
val compile_function : ast * location -> opcode list
val run : ast * location -> opcode list
