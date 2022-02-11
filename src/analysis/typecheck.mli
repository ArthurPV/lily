open Lily_parser.Ast

val can_check_type : ast -> bool
val check_generics_type : ast -> unit
val check_expr_type : expr -> unit
val check_type : ast -> unit