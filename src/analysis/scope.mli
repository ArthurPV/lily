open Lily_lexer.Location
open Lily_parser.Ast
open Lily_parser.Parser
open Buffer

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
[@@deriving show]

type scope_access =
  [ `Fun of from_access * string * argument array * location * ast option
    [@printer
      fun fmt f ->
        let args = match f with _, _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length args then
            loop ~i:(i + 1) ~l:(show_argument args.(i))
          else String.concat ", " l
        in
        fprintf fmt "Fun(%s, %s, %s, %s, %s)"
          (match f with acc, _, _, _, _ -> show_from_access acc)
          (match f with _, s, _, _, _ -> s)
          (loop ())
          (match f with _, _, _, loc, _ -> show_location loc)
          (match f with
          | _, _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* function access => function call *)
    `Identifier of
    from_access * string * location * ast option
    [@printer
      fun fmt i ->
        fprintf fmt "Identifier(%s, %s, %s, %s)"
          (match i with acc, _, _, _ -> show_from_access acc)
          (match i with _, s, _, _ -> s)
          (match i with _, _, loc, _ -> show_location loc)
          (match i with
          | _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* identifier => (variable access, constant access module access, type
       access) *)
    `Type of
    from_access * string * data_type array * location * ast option
    [@printer
      fun fmt t ->
        let arr_dt = match t with _, _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length arr_dt then
            loop ~i:(i + 1) ~l:(show_data_type arr_dt.(i) :: l) ()
          else String.concat ", " l
        in
        fprintf fmt "Type(%s, %s, %s, %s, %s)"
          (match t with acc, _, _, _, _ -> show_from_access acc)
          (match t with _, s, _, _, _ -> s)
          (loop ())
          (match t with _, _, _, loc, _ -> show_location loc)
          (match t with
          | _, _, _, _, op -> (
              match op with Some v -> show_ast v | None -> "None"))]
  | (* type access => alias, record, enum, class *)
    `Variant of
    from_access * string array * location * variant
    [@printer
      fun fmt v ->
        let arr = match v with _, arr, _, _ -> arr in
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length arr then loop ~i:(i + 1) ~l:(arr.(i) :: l)
          else String.concat ", " l
        in
        fprintf fmt "Variant(%s, %s, %s, %s)"
          (match v with acc, _, _, _ -> show_from_access acc)
          (loop ())
          (match v with _, _, loc, _ -> show_location loc)
          (match v with _, _, _, v -> show_variant v)]
  | (* variant access => variant call *)
    `IdentifierAddr of scope_access array
    [@printer
      fun fmt acc ->
        let rec loop ?(i = 0) ?(l = []) () =
          if i < Array.length acc then
            loop ~i:(i + 1) ~l:(show_scope_access acc.(i) :: l) ()
          else String.concat ", " l
        in
        fprintf fmt "IdentifierAddr(%s)" (loop ())] ]
(* identifier addr => identifier access *)
[@@deriving show]

type scope = {
  parser : parser;
  buffer : scope buffer;
  mutable global : scope_access array;
  mutable global_pub : scope_access array;
  mutable used : scope_access array;
  mutable idx_of_main_fun : int;
}

val new_scope : parser -> scope
val push_used : scope -> scope_access -> unit
val emit_unused : scope -> scope_access -> unit
val verify_if_used : scope -> unit

val run_import :
  scope -> path:string -> as_value:string -> is_pub:bool -> location -> unit

val resolve_import :
  scope -> value:string -> as_value:string -> is_pub:bool -> location -> unit

val resolve_all_imports : scope -> unit

(* check global scope *)
val get_global_access :
  scope -> (ast * location) array -> p_pub:bool -> scope_access array

val verify_if_same_access : scope -> scope_access array -> int
val is_contain_main_fun : scope -> bool * int

(* check scope *)
(* add reference of value in nodes *)
val add_ref_on_node : expr -> ast option -> expr

val check_expr :
  scope -> ast ref -> location -> scope_access array array -> ast

val search_in :
  scope ->
  scope_access array ->
  pos:int ->
  value:expr ->
  location ->
  scope_access option * scope_access array option

val identifier_access_node_to_identifier_addr :
  scope -> ast -> location -> scope_access

val push_access_in :
  scope_access array ref array ref -> int -> scope_access -> unit

val check_duplicate_argument_name : scope -> argument array -> unit

val check_count_argument :
  scope -> argument array -> args_fun_call array -> location -> unit

val get_argument_access :
  scope -> argument array -> args_fun_call array -> scope_access array

val check_fun_scope :
  scope ->
  argument array ->
  args_fun_call array ->
  scope_access array ref array ->
  (ast * location) array ->
  location option ->
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
