open Opcode

module VMValue = struct
    type t =
      | Int of Stdint.int64 [@printer fun fmt i -> fprintf fmt "Int(%s)" (Stdint.Int64.to_string i)]
      | Float of float
      | Bool of bool
      | String of string
      | Char of char
      | Undef
      | Nil
    [@@deriving show]

    let to_int = function
      | Int i -> i
      | _ -> failwith "unreachable"

    let to_float = function
      | Float f -> f
      | _ -> failwith "unreachable"

    let to_bool = function
      | Bool b -> b
      | _ -> failwith "unreachable"

    let to_string = function
      | String s -> s
      | _ -> failwith "unreachable"

    let to_char = function
      | Char c -> c
      | _ -> failwith "unreachable"
end

module Chunk = struct
  type t = {
    code : opcode array;
    constants : VMValue.t array;
  }

  let new_chunk = { code = [||]; constants = [||]; }

  let get_constant t idx =
    assert(idx < Array.length t.constants);
    t.constants.(idx)
end

let stack_size = 256

type vm = {
  chunk : Chunk.t;
  ip : Stdint.uint8;
  stack : VMValue.t array;
}
