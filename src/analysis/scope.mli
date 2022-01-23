open Lily_lexer.Location
open Lily_parser.Ast
open Lily_parser.Parser

type from_access =
  [ `Fun
  | `Constant
  | `Module
  | `Alias
  | `Record
  | `Enum
  | `Variant
  | `Class
  | `Trait
  | `None ]

type scope_access =
  [ `Fun of from_access * string * argument array * location * ast option
  | (* function access => function call *)
    `Identifier of
      from_access * string * location * ast option
  | (* identifier => (variable access, constant access module access, type
       access) *)
    `Type of
      from_access * string * data_type array * location * ast option
  | (* type access => alias, record, enum, class *)
    `Variant of
      from_access * string array * location * variant
  | (* variant access => variant call *)
    `IdentifierAddr of
      scope_access array
      (* identifier addr => identifier access *) ]

type scope = {
  parser : parser;
  mutable global : scope_access array;
  mutable global_pub : scope_access array;
  mutable used : scope_access array;
}

val new_scope : parser -> scope

(* check global scope *)
val get_global_access :
  scope -> (ast * location) array -> p_pub:bool -> scope_access array

val verify_if_same_access : scope -> scope_access array -> int
val is_contain_main_fun : scope -> bool * int

(* check scope *)
(* add reference of value in nodes *)
val add_ref_on_node : expr -> ast option -> expr
val check_expr : scope -> ast ref -> location -> scope_access array array -> ast

val check_fun_scope :
  scope ->
  argument array ->
  scope_access array array ->
  (ast * location) array ->
  unit

val check_alias_scope : scope -> (ast * location) array -> unit
val check_record_scope : scope -> (ast * location) array -> unit
val check_enum_scope : scope -> (ast * location) array -> unit
val check_variable_scope : scope -> (ast * location) array -> unit
val check_constant_scope : scope -> (ast * location) array -> unit
val check_module_scope : scope -> (ast * location) array -> unit
val check_block_scope : scope -> (ast * location) array -> unit
val check_scope : scope -> (ast * location) array -> unit
val run : scope -> unit
