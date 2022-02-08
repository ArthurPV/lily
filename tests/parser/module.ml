module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
module Parser = Lily_parser.Parser
open Lily_parser.Ast

let test () =
  let parser =
    "module calc =\n\
     fun add(x, y) = x+y end\n\
     fun sub(x, y) = x-y end\n\
     end\n\n\
     pub module calc2 =\n\
     fun add(x, y) = x+y end\n\
     fun sub(x, y) = x-y end\n\
     end\n" |> Source.new_source "" |> Lexer.new_lexer |> Parser.new_parser
  in
  Parser.run parser;
  let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

  Alcotest.(check string)
    "same string"
    "Ast.Module {id = \"calc\";\n\
    \  body =\n\
    \  [Ast.Fun {id = sub; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = None; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = None; loc = Location }];\n\
    \  return_type = None; body = [(x, None) - (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}, Ast.Fun {id = \
     add; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = None; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = None; loc = Location }];\n\
    \  return_type = None; body = [(x, None) + (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}];\n\
    \  is_pub = false; is_test = false}"
    (show_ast nodes.(0));

  Alcotest.(check string)
    "same string"
    "Ast.Module {id = \"calc2\";\n\
    \  body =\n\
    \  [Ast.Fun {id = sub; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = None; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = None; loc = Location }];\n\
    \  return_type = None; body = [(x, None) - (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}, Ast.Fun {id = \
     add; poly_args = [];\n\
    \  args =\n\
    \  [{ Ast.id = x; kind = Normal; data_type = None; loc = Location }, { \
     Ast.id = y; kind = Normal; data_type = None; loc = Location }];\n\
    \  return_type = None; body = [(x, None) + (y, None)]; is_pub = False;\n\
    \  is_async = False; is_test = False; is_export = False}];\n\
    \  is_pub = true; is_test = false}"
    (show_ast nodes.(1))
