open Lily_parser.Ast
open Lily_analysis.Scope

module LIR : sig
    type t =
      | Int of Stdint.int64 [@printer fun fmt i -> fprintf fmt "Int(%s)" (Stdint.Int64.to_string i)]
      | Float of float
      | Bool of bool
      | String of string
      | Char of char
      | Unit
      | Object
      | Fun of string * t array (* TODO: add args *)
      | Variable of string * t
      | Constant of string * t
      | Block of t option * t array
      | Call of t array * t
      | Ret of t
      | Undef
      | Nil
    [@@deriving show]

    val to_int : t -> Stdint.int64
    val to_float : t -> float
    val to_bool : t -> bool
    val to_string : t -> string
    val to_char : t -> char
end

type compiler = {
    scope: scope;
    nodes_value: LIR.t array;
}

val compile_expr : ast -> LIR.t
val compile_fun : decl -> LIR.t
val compile_class : decl -> LIR.t
val run : compiler -> unit
