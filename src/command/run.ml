module Cli = Lily_common.Cli
module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module Token = Lily_lexer.Token
module Parser = Lily_parser.Parser
module Scope = Lily_analysis.Scope

let run_bytecode filename =
  let compile_time = Sys.time () in
  match Source.read_file filename with
  | Ok content ->
      let scope =
        content
        |> Source.new_source filename
        |> Lexer.new_lexer |> Parser.new_parser |> Scope.new_scope
      in
      Scope.run scope;
      Printf.printf "Compilation time in %f secs\n"
        (Sys.time () -. compile_time)
  | Error err -> Cli.print_cli_error (Cli.show_cli_error_kind err)
