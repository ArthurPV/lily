open Argument
open Lily_common.Error

type command =
  | Build
  | Compile
  | Help
  | Init
  | New
  | Repl
  | Run
  | Test
  | To
  | Version

val parse : argument -> (command option, cli_error_kind) result

val run :
  argument -> (argument -> (command option, cli_error_kind) result) -> unit
