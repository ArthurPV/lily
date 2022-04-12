open Lily_parser.Ast
open Lily_parser.Parser
open Lily_lexer.Location

type typecheck = parser

val can_check_type : ast * location -> bool
val check_generics_type : typecheck -> ast * location -> data_type
val check_arithmetic_expr : data_type * data_type -> data_type
val check_logical_expr : data_type * data_type -> data_type

val check_expr_type :
  typecheck ->
  specified:data_type option ->
  neg:bool ->
  expr * location ->
  data_type

val check_type :
  typecheck ->
  specified:data_type option ->
  neg:bool ->
  ast * location ->
  data_type

val check_tuple_type :
  typecheck -> expr * location -> specified:data_type option -> data_type

val check_fun_args_type : argument array -> ast
val check_fun_type : ast -> ast
val check_constant_type : typecheck -> ast * location -> unit
