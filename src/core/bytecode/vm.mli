open Opcode

module VMValue : sig
    type t =
      | Int of Stdint.int64 [@printer fun fmt i -> fprintf fmt "Int(%s)" (Stdint.Int64.to_string i)]
      | Float of float
      | Bool of bool
      | String of string
      | Char of char
      | Undef
      | Nil
    [@@deriving show]

    val to_int : t -> Stdint.int64
    val to_float : t -> float
    val to_bool : t -> bool
    val to_string : t -> string
    val to_char : t -> char
end

module Chunk : sig
  type t = {
    code : opcode array;
    constants : VMValue.t array;
  }

  val new_chunk : t
  val get_constant : t -> int -> VMValue.t
end

val stack_size : int

type vm = {
  chunk : Chunk.t;
  ip : Stdint.uint8;
  stack : VMValue.t array;
}
