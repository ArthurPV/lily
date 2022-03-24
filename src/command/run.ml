module CliError = Lily_common.Error
module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module Token = Lily_lexer.Token
module Ast = Lily_parser.Ast
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
        (Sys.time () -. compile_time);
      Array.iter
        (fun y -> Printf.printf "%s\n" (Scope.show_scope_access y))
        scope.global_pub
      (* Array.iter (fun n -> Printf.printf "%s\n" (Ast.show_ast n))
         (scope.parser.nodes |> Array.map (fun (x, _) -> x)) *)
  | Error err -> CliError.print_cli_error (CliError.show_cli_error_kind err)
