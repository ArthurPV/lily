module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module Parser = Lily_parser.Parser
open Lily_parser.Ast

let test () =
  let parser =
    "A Int8 := 3\npub B Int8 := 30\n" |> Source.new_source ""
    |> Lexer.new_lexer |> Parser.new_parser
  in
  Parser.run parser;
  let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

  Alcotest.(check string)
    "same string"
    "Ast.Constant {id = A; data_type = `I8; expr = 3; is_pub = False}"
    (show_ast nodes.(0));

  Alcotest.(check string)
    "same string"
    "Ast.Constant {id = B; data_type = `I8; expr = 30; is_pub = True}"
    (show_ast nodes.(1))
