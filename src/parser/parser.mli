module Diagnostic = Lily_lexer.Diagnostic
open Lily_lexer.Lexer
open Lily_lexer.Location
open Lily_lexer.Token
open Ast

type parser = {
  lexer : lexer;
  mutable pos : int;
  mutable nodes : (ast * location) array;
  mutable errors : Diagnostic.diagnostic array;
  mutable current_token : token;
  mutable previous_token : token;
  mutable current_location : location;
  mutable previous_location : location;
}

val new_parser : lexer -> parser
val ast_of_nodes : parser -> idx:int -> ast
val loc_of_nodes : parser -> idx:int -> location

val change_nodes_visibility :
  (ast * location) array -> visibility:bool -> (ast * location) array

val new_diagnostic :
  parser ->
  Diagnostic.diagnostic_kind ->
  string ->
  location ->
  Diagnostic.diagnostic

val advance : parser -> add_pos:bool -> unit
val next_token : parser -> unit
val expect_token : parser -> token -> exn -> unit
val matches : parser -> token -> bool
val is_eof : parser -> bool
val peek_token : parser -> n:int -> token option
val parse_data_type : parser -> data_type
val parse_fun_data_type : parser -> data_type
val is_binop : parser -> n:int -> bool
val is_data_type : parser -> n:int -> bool
val run : parser -> unit

(* parse function *)
(* expr *)
val parse_assign : parser -> location -> expr
val parse_logical_or : parser -> expr
val parse_logical_and : parser -> expr
val parse_equality : parser -> expr
val parse_comparison : parser -> expr
val parse_range : parser -> expr
val parse_term : parser -> expr
val parse_factor : parser -> expr
val parse_exp : parser -> expr
val parse_unary : parser -> expr
val parse_grouping : parser -> is_mut:bool -> ast
val parse_function_call : parser -> id:expr -> expr
val parse_class_call : parser -> expr
val parse_record_call : parser -> id:expr -> expr
val parse_anonymous_function : parser -> expr
val parse_identifier_access : parser -> expr -> expr
val parse_self_access : parser -> expr
val parse_tuple : parser -> expr
val parse_array : parser -> expr
val parse_array_access : parser -> id:expr -> expr
val parse_tuple_access : parser -> id:expr -> expr
val parse_variant : parser -> id:expr -> expr
val parse_primary_expr : parser -> expr
val parse_expr2 : parser -> expr

(* decl *)
val parse_variable : parser -> id:string -> is_mut:bool -> ast
val parse_constant : parser -> id:string -> is_pub:bool -> ast
val parse_multiple : parser -> (string * data_type option) array * bool

val parse_multiple_variable :
  parser -> ids:(string * data_type option) array -> is_mut:bool -> ast array

val parse_multiple_constant :
  parser -> ids:(string * data_type option) array -> is_pub:bool -> ast array

val parse_function :
  parser ->
  is_pub:bool ->
  is_async:bool ->
  is_test:bool ->
  is_export:bool ->
  decl

val parse_body_module : parser -> (ast * location) array
val parse_module : parser -> is_pub:bool -> is_test:bool -> decl
val parse_type : parser -> is_pub:bool -> decl
val parse_alias : parser -> string -> poly_args_kind array -> is_pub:bool -> decl
val parse_record : parser -> string -> poly_args_kind array -> is_pub:bool -> decl
val parse_enum : parser -> string -> poly_args_kind array -> is_pub:bool -> decl
val parse_object : parser -> is_pub:bool -> decl
val parse_property : parser -> is_pub:bool -> decl
val parse_body_class : parser -> string -> (ast * location) array

val parse_class :
  parser -> string -> poly_args_kind array -> expr array -> is_pub:bool -> decl

val parse_method : parser -> string -> is_pub:bool -> decl
val parse_trait : parser -> string -> poly_args_kind array -> is_pub:bool -> decl
val parse_import : parser -> is_pub:bool -> decl
val parse_pub_block : parser -> decl * location
val parse_pub : parser -> decl

val parse_body :
  parser ->
  closure:token option * token option * token option ->
  (ast * location) array

val parse_polymorphic_argument : parser -> poly_args_kind array
val parse_argument : parser -> argument array
val parse_method_argument : parser -> argument_method array
val parse_decl : parser -> ast

(* stmt *)
val parse_if : parser -> stmt
val parse_match : parser -> stmt
val parse_try : parser -> stmt
val parse_return : parser -> stmt
val parse_await : parser -> stmt
val parse_while : parser -> stmt
val parse_for : parser -> stmt
