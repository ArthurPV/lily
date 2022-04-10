open Lily_parser.Ast
open Lily_lexer.Location
open Stack
open Opcode

type vm = {
  main_node : ast * location;
  stack : value stack;
  reg : (Stdint.uint8, value) Hashtbl.t;
}

val new_vm : ast * location -> vm
val run : vm -> unit
