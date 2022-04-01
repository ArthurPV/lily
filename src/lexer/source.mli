open Lily_common.Error

type source = {
  filename : string;
  content : string; (* Content of filename *)
  mutable c : char;
      (* A character from the position in the content (content.[pos]) *)
  len : int; (* Length of content *)
  mutable pos : int; (* Position in content; default: 0 *)
}

val new_source : string -> string -> source
(** [new_source filename content] Init source type. *)

val read_file : string -> (string, cli_error_kind) result
(** [read_file filename] Read file from filename. Return Error cli_error_kind
    if the [filename] is not found or if is a directory or if has a bad
    extension else return Ok content. *)
