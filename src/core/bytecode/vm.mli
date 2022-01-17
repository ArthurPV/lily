open Opcode
open Compile

module Chunk : sig
  type t = {
    code : opcode array;
    constants : LIR.t array; (* change that *)
  }

  val new_chunk : t
  val get_constant : t -> int -> LIR.t
end

val stack_size : int

type vm = {
  chunk : Chunk.t;
  ip : Stdint.uint8;
  stack : LIR.t array; (* change that *)
}
