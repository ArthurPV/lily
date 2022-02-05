module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module Parser = Lily_parser.Parser
open Lily_parser.Ast

(* Weird bug on argument location *)
let test () =
  let parser =
    "fun add(x, y) = x+y end\n\
     fun sub(x Int8, y Int8) Int8 = x-y end\n\
     fun get(a=10) = a end\n" |> Source.new_source "" |> Lexer.new_lexer
    |> Parser.new_parser
  in
  Parser.run parser;
  let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

  Alcotest.(check string)
    "same string"
    "Ast.Fun {id = add; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = None; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = None; loc = Location }];\n\
    \  return_type = None; body = [(x, None) + (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}"
    (show_ast nodes.(0));

  Alcotest.(check string)
    "same string"
    "Ast.Fun {id = sub; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = `I8; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = `I8; loc = Location }];\n\
    \  return_type = `I8; body = [(x, None) - (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}"
    (show_ast nodes.(1));

  Alcotest.(check string)
    "same string"
    "Ast.Fun {id = get; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = a; kind = Default: 10; data_type = None; loc = Location \
     }];\n\
    \  return_type = None; body = [(a, None)]; is_pub = False; is_async = \
     False;\n\
    \  is_test = False; is_export = False}"
    (show_ast nodes.(2))
