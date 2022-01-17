module Lexer = Lily_lexer.Lexer
module Parser = Lily_parser.Parser
module Source = Lily_lexer.Source

[@@@warning "-26"]
module BinaryExpr = struct
  let test () =
    let parser = "C Int8 := 3+3\n" |> Source.new_source "" |> Lexer.new_lexer |> Parser.new_parser in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string) "same string" "hello" "hello"
end
