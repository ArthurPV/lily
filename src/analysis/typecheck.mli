open Lily_parser.Ast
open Lily_parser.Parser
open Lily_lexer.Location

type typecheck = parser

val can_check_type : ast * location -> bool
val check_generics_type : typecheck -> ast * location -> data_type
val check_arithmetic_expr : data_type * data_type -> data_type
val check_logical_expr : data_type * data_type -> data_type
val check_expr_type : typecheck -> expr * location -> data_type
val check_type : typecheck -> ast * location -> data_type
val check_fun_args_type : argument array -> ast
val check_fun : ast -> ast
