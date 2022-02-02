open Argument
open Lily_common.Error

module BuildOptions : sig
  type path = string
  type t_kind = Help | Input of path | None | Err of cli_error_kind
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val get_error : t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module CompileOptions : sig
  type path = string

  type t_kind =
    | Help
    | Input of path
    | None
    | DumpLexer
    | DumpParser
    | DumpAST
    | DumpIR
    | Object

  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module InitOptions : sig
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module NewOptions : sig
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module RunOptions : sig
  type path = string
  type t_kind = Help | None | Input of path | Err of cli_error_kind
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val get_error : t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module TestOptions : sig
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end

module ToOptions : sig
  type to_language_kind = Js | Ml | Rb | C
  type path = string
  type t_kind = Help | None | Input of path | Required of to_language_kind
  type t = { mutable op_arr : t_kind array }

  val new_t : t
  val parse : argument -> t -> unit
  val run : ?i:int -> argument -> t -> (argument -> t -> unit) -> unit
end
