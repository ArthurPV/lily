open Lily_parser.Ast
open Lily_lexer.Token
open Scope

type buffer = {
  mutable filenames : string array;
  mutable contents : string array;
  mutable tokens : token array array;
  (* mutable locations : location array array; *)
  mutable nodes : ast array array;
  mutable scopes : scope array;
}

let new_buffer =
  {
    filenames = [||];
    contents = [||];
    tokens = [||];
    (* locations = [||]; *)
    nodes = [||];
    scopes = [||];
  }

let is_same_filename buffer filename =
  let same = ref false in
  let rec loop ?(i = 0) () =
    if i < Array.length buffer.filenames then
      if filename = buffer.filenames.(i) then same := true
      else loop ~i:(i + 1) ()
  in
  loop ();
  !same

let push_buffer buffer ~filename ~content tokens nodes scope =
  buffer.filenames <- Array.append buffer.filenames [| filename |];
  buffer.contents <- Array.append buffer.contents [| content |];
  buffer.tokens <- Array.append buffer.tokens [| tokens |];
  buffer.nodes <- Array.append buffer.nodes [| nodes |];
  buffer.scopes <- Array.append buffer.scopes [| scope |]
