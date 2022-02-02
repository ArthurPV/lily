open Argument
open Options
open Lily_common.Error
open Lily_command.Help
open Lily_command.Version

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

let parse arg =
  if arg.argc > 1 then
    match arg.argv.(1) with
    | "build" -> Ok (Some Build)
    | "compile" -> Ok (Some Compile)
    | "help" | "-h" | "--help" -> Ok (Some Help)
    | "init" -> Ok (Some Init)
    | "new" -> Ok (Some New)
    | "repl" -> Ok (Some Repl)
    | "run" -> Ok (Some Run)
    | "test" -> Ok (Some Test)
    | "to" -> Ok (Some To)
    | "version" | "-v" | "--version" -> Ok (Some Version)
    | "" -> Ok None
    | s -> Error (UnknownCommand s)
  else Ok None

[@@@warning "-27"]

let run arg res =
  match res arg with
  | Ok (Some c) -> (
      match c with
      | Build ->
          BuildOptions.parse |> BuildOptions.run arg BuildOptions.new_t
      | Compile ->
          CompileOptions.parse |> CompileOptions.run arg CompileOptions.new_t
      | Help -> Printf.printf "%s" main_help
      | Init -> InitOptions.parse |> InitOptions.run arg InitOptions.new_t
      | New -> NewOptions.parse |> NewOptions.run arg NewOptions.new_t
      | Repl -> Printf.printf "repl"
      | Run -> RunOptions.parse |> RunOptions.run arg RunOptions.new_t
      | Test -> TestOptions.parse |> TestOptions.run arg TestOptions.new_t
      | To -> ToOptions.parse |> ToOptions.run arg ToOptions.new_t
      | Version -> Printf.printf "lily v%s\n" lily_version)
  | Ok None -> ()
  | Error e -> e |> show_cli_error_kind |> print_cli_error
