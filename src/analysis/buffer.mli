open Lily_lexer.Token
open Lily_parser.Ast
open Scope

type buffer = {
  mutable filenames : string array;
  mutable contents : string array;
  mutable tokens : token array array;
  (* mutable locations : location array array; *)
  mutable nodes : ast array array;
  mutable scopes : scope array;
}

val new_buffer : buffer
val is_same_filename : buffer -> string -> bool
val push_buffer : buffer -> filename:string -> content:string -> token array -> ast array -> scope -> unit
