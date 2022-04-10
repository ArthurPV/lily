open Lily_parser.Ast
open Lily_lexer.Location
open Stack
open Opcode

type vm = {
  main_node : ast * location;
  stack : value stack;
  reg : (Stdint.uint8, value) Hashtbl.t;
}

[@@@warning "-27"]

let new_vm main_node =
  { main_node; stack = { st = [] }; reg = Hashtbl.create 0 }

let run vm = ()
