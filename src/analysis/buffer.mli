open Lily_lexer.Token
open Lily_parser.Ast

type 'a buffer = {
  mutable filenames : string array;
  mutable contents : string array;
  mutable tokens : token array array;
  (* mutable locations : location array array; *)
  mutable nodes : ast array array;
  mutable scopes : 'a array;
}

val new_buffer : 'a array -> 'a buffer
val is_same_filename : 'a buffer -> string -> bool

val push_buffer :
  'a buffer ->
  filename:string ->
  content:string ->
  token array ->
  ast array ->
  'a ->
  unit
