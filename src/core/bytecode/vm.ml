open Opcode
open Compile

module Chunk = struct
  type t = { code : opcode array; constants : LIR.t array (* change that *) }

  let new_chunk = { code = [||]; constants = [||] }

  let get_constant t idx =
    assert (idx < Array.length t.constants);
    t.constants.(idx)
end

let stack_size = 256

type vm = {
  chunk : Chunk.t;
  ip : Stdint.uint8;
  stack : LIR.t array; (* change that *)
}
