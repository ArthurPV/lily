open Lily_cli.Argument
open Lily_cli.Commands

let () =
  let arg = new_argument in
  parse |> run arg
