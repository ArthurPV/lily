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
  | `None ]

type scope_access =
  [ `Fun of
    from_access * string * argument array * location
    (* function access => function call *)
  | `Identifier of
    from_access * string * location
    (* identifier => (variable access, constant access module access, type
       access) *)
  | `Type of
    from_access * string * data_type array * location
    (* type access => alias, record, enum, class *)
  | `Variant of
    from_access * string array * location
    (* variant access => variant call *)
  | `IdentifierAddr of
    scope_access array
    (* identifier addr => identifier access *) ]

type scope = {
    parser : parser;
    mutable global : scope_access array;
    mutable global_pub : scope_access array;
  }

val new_scope : parser -> scope

(*  check global scope *)
val get_global_access : scope -> (ast * location) array -> p_pub:bool -> scope_access array
val verify_if_same_access : scope -> scope_access array -> int

(* check scope *)
val if_verify_scope_value : ast -> (bool * ast array)
val check_expr : scope -> scope_access array array -> unit
val check_fun_scope : scope -> argument array -> scope_access array array -> (ast * location) array -> unit
val check_alias_scope : scope -> (ast * location) array -> unit
val check_record_scope : scope -> (ast * location) array -> unit
val check_enum_scope : scope -> (ast * location) array -> unit
val check_variable_scope : scope -> (ast * location) array -> unit
val check_constant_scope : scope -> (ast * location) array -> unit
val check_module_scope : scope -> (ast * location) array -> unit
val check_block_scope : scope -> (ast * location) array -> unit
val check_scope : scope -> (ast * location) array -> unit

val run : scope -> unit
