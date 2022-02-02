open Lily_common.Error

type source = {
  filename : string;
  content : string;
  mutable c : char;
  len : int;
  mutable pos : int;
}

val new_source : string -> string -> source
val read_file : string -> (string, cli_error_kind) result
