open Lily_parser.Ast
open Lily_lexer.Location
open Opcode

val compile_expr :
  dt:data_type option ->
  (expr * location) option ->
  opcode list ->
  opcode list

val compile_integer : dt:data_type option -> expr -> opcode
val compile_variable : ast * location -> opcode list
val compile_function : ast * location -> opcode list
val run : ast * location -> opcode list
