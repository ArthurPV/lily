module Lexer = Lily_lexer.Lexer
module Parser = Lily_parser.Parser
module Source = Lily_lexer.Source
open Lily_parser.Ast

module BinaryExpr = struct
  let test () =
    let parser =
      "A Int8 := (3+3)+3\n\
       B Int8 := +3\n\
       C Int8 := -3\n\
       D Bool := not True\n\
       E Int8 := 3*3\n\
       F Int8 := 3/3\n\
       G Int8 := 3%3\n\
       H Int8 := 3+3\n\
       I Int8 := 3-3\n\
       J Int8 := 3 ^ 3\n\
       K Int8 := 3..5\n\
       L Bool := 3 < 5\n\
       M Bool := 3 > 5\n\
       N Bool := 3 <= 5\n\
       O Bool := 3 >= 5\n\
       P Bool := 3 == 5\n\
       Q Bool := 3 != 5\n\
       R Bool := 3 < 5 and 3 > 5\n\
       S Int8 := s = 3\n\
       T Int8 := s += 3\n\
       U Int8 := s -= 3\n\
       V Int8 := s *= 3\n\
       W Int8 := s /= 3\n\
       X Int8 := s %= 3\n\
       Y Int8 := s ^= 3\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = (3 + 3) + 3; is_pub = \
       False}"
      (show_ast nodes.(0));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `I8; expr = +3; is_pub = False}"
      (show_ast nodes.(1));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = C; data_type = `I8; expr = -3; is_pub = False}"
      (show_ast nodes.(2));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = D; data_type = `Bool; expr = not True; is_pub = \
       False}"
      (show_ast nodes.(3));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = E; data_type = `I8; expr = 3 * 3; is_pub = False}"
      (show_ast nodes.(4));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = F; data_type = `I8; expr = 3 / 3; is_pub = False}"
      (show_ast nodes.(5));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = G; data_type = `I8; expr = 3 % 3; is_pub = False}"
      (show_ast nodes.(6));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = H; data_type = `I8; expr = 3 + 3; is_pub = False}"
      (show_ast nodes.(7));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = I; data_type = `I8; expr = 3 - 3; is_pub = False}"
      (show_ast nodes.(8));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = J; data_type = `I8; expr = 3 ^ 3; is_pub = False}"
      (show_ast nodes.(9));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = K; data_type = `I8; expr = 3..5; is_pub = False}"
      (show_ast nodes.(10));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = L; data_type = `Bool; expr = 3 < 5; is_pub = \
       False}"
      (show_ast nodes.(11));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = M; data_type = `Bool; expr = 3 > 5; is_pub = \
       False}"
      (show_ast nodes.(12));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = N; data_type = `Bool; expr = 3 <= 5; is_pub = \
       False}"
      (show_ast nodes.(13));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = O; data_type = `Bool; expr = 3 >= 5; is_pub = \
       False}"
      (show_ast nodes.(14));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = P; data_type = `Bool; expr = 3 == 5; is_pub = \
       False}"
      (show_ast nodes.(15));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = Q; data_type = `Bool; expr = 3 != 5; is_pub = \
       False}"
      (show_ast nodes.(16));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = R; data_type = `Bool; expr = 3 < 5 and 3 > 5;\n\
      \  is_pub = False}"
      (show_ast nodes.(17));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = S; data_type = `I8; expr = (s, None) = 3; is_pub \
       = False}"
      (show_ast nodes.(18));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = T; data_type = `I8; expr = (s, None) += 3; is_pub \
       = False}"
      (show_ast nodes.(19));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = U; data_type = `I8; expr = (s, None) -= 3; is_pub \
       = False}"
      (show_ast nodes.(20));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = V; data_type = `I8; expr = (s, None) *= 3; is_pub \
       = False}"
      (show_ast nodes.(21));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = W; data_type = `I8; expr = (s, None) /= 3; is_pub \
       = False}"
      (show_ast nodes.(22));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = X; data_type = `I8; expr = (s, None) %= 3; is_pub \
       = False}"
      (show_ast nodes.(23));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = Y; data_type = `I8; expr = (s, None) ^= 3; is_pub \
       = False}"
      (show_ast nodes.(24))

  let complex_test () =
    let parser =
      "A Int8 := 3+3-3+(3*3)/3\n\
       B Int8 := 3*3*5*2/3*(3-3)+(3-2)+4\n\
       C Int8 := (3+(3+(3+3)+3)+3)+3\n\
       D Bool := (5 == 10 and 5 >= 10) or 3 < 10\n\
       E Int8 := - 3 + 3 - 3 + - 3\n\
       F Int8 := (3+3) * (3+3)\n\
       G Int8 := 3*3^3\n\
       H Int8 := 3 % 3 ^ 3 * 3 / 2\n\
      \ I Int8 := 3 ** this is a comment\n\
      \ * 3\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = 3 + 3 - 3 + (3 * 3) / 3;\n\
      \  is_pub = False}"
      (show_ast nodes.(0));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `I8;\n\
      \  expr = 3 * 3 * 5 * 2 / 3 * (3 - 3) + (3 - 2) + 4; is_pub = False}"
      (show_ast nodes.(1));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = C; data_type = `I8;\n\
      \  expr = (3 + (3 + (3 + 3) + 3) + 3) + 3; is_pub = False}"
      (show_ast nodes.(2));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = D; data_type = `Bool;\n\
      \  expr = (5 == 10 and 5 >= 10) or 3 < 10; is_pub = False}"
      (show_ast nodes.(3));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = E; data_type = `I8; expr = -3 + 3 - 3 + -3; \
       is_pub = False}"
      (show_ast nodes.(4));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = F; data_type = `I8; expr = (3 + 3) * (3 + 3);\n\
      \  is_pub = False}"
      (show_ast nodes.(5));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = G; data_type = `I8; expr = 3 * 3 ^ 3; is_pub = \
       False}"
      (show_ast nodes.(6));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = H; data_type = `I8; expr = 3 % 3 ^ 3 * 3 / 2;\n\
      \  is_pub = False}"
      (show_ast nodes.(7));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = I; data_type = `I8; expr = 3 * 3; is_pub = False}"
      (show_ast nodes.(8))
end

module CallExpr = struct
  let test () =
    let parser =
      "A Int8 := add(3, 4)\n\
       B Person := Person { name = \"Theresa\", age = 20 }\n\
       C Animal := new Animal(\"Tiger\")\n\
       D Int8 := Calc.add(3,4)\n\
       E Animal := new R.Animal(\"Tiger\")\n" |> Source.new_source ""
      |> Lexer.new_lexer |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = (add, None)(3, 4);\n\
      \  is_pub = False}"
      (show_ast nodes.(0));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `CustomType ((\"Person\", None));\n\
      \  expr = (Person, None) { (name, Theresa), (age, 20) }; is_pub = \
       False}"
      (show_ast nodes.(1));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = C; data_type = `CustomType ((\"Animal\", None));\n\
      \  expr = new (Animal, None)(Tiger); is_pub = False}"
      (show_ast nodes.(2));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = D; data_type = `I8;\n\
      \  expr = ((Calc, None).(add, None), None)(3, 4); is_pub = False}"
      (show_ast nodes.(3));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = E; data_type = `CustomType ((\"Animal\", None));\n\
      \  expr = new ((R, None), None)(Tiger); is_pub = False}"
      (show_ast nodes.(4))
end

module AnonymousFunExpr = struct
  let test () =
    let parser =
      "A Int8 := fun (x, y) => x+y end\nB Int8 := fun (x=3) => x end\n"
      |> Source.new_source "" |> Lexer.new_lexer |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8;\n\
      \  expr =\n\
      \  fun({ Ast.id = x; kind = Normal; data_type = None; loc = Location \
       }, { Ast.id = y; kind = Normal; data_type = None; loc = Location }) \
       =>\n\
       (x, None) + (y, None)\n\
       end;\n\
      \  is_pub = False}"
      (show_ast nodes.(0));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `I8;\n\
      \  expr =\n\
      \  fun({ Ast.id = x; kind = Default: 3; data_type = None; loc = \
       Location }) =>\n\
       (x, None)\n\
       end;\n\
      \  is_pub = False}"
      (show_ast nodes.(1))
end

module IdentifierExpr = struct
  let test () =
    let parser =
      "A Int8 := a\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = (a, None); is_pub = \
       False}"
      (show_ast nodes.(0))
end

module TupleExpr = struct
  let test () =
    let parser =
      "A Int8 := (1, False, 2, True)\n\
      \       (B Uint8, C Uint8, D Uint8) := (1, 2, 3)\n"
      |> Source.new_source "" |> Lexer.new_lexer |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = (1, False, 2, True);\n\
      \  is_pub = False}"
      (show_ast nodes.(0));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `U8; expr = 3; is_pub = False}"
      (show_ast nodes.(1));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = C; data_type = `U8; expr = 2; is_pub = False}"
      (show_ast nodes.(2));
    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = D; data_type = `U8; expr = 1; is_pub = False}"
      (show_ast nodes.(3))
end

module ArrayExpr = struct
  let test () =
    let parser =
      "A [Int8] := [1, 2, 3]\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `Array (`I8); expr = [1, 2, 3];\n\
      \  is_pub = False}"
      (show_ast nodes.(0))
end

module VariantExpr = struct
  let test () =
    let parser =
      "A Letter := Letter.A(1)\nB Person := Person.Age(2)\n"
      |> Source.new_source "" |> Lexer.new_lexer |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `CustomType ((\"Letter\", None));\n\
      \  expr = ((Letter, None).(A, None), None)(1); is_pub = False}"
      (show_ast nodes.(0));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `CustomType ((\"Person\", None));\n\
      \  expr = ((Person, None).(Age, None), None)(2); is_pub = False}"
      (show_ast nodes.(1))
end

module LiteralExpr = struct
  let test () =
    let parser =
      "A Int8 := 3\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `I8; expr = 3; is_pub = False}"
      (show_ast nodes.(0))
end

module OtherExpr = struct
  let test () =
    let parser =
      "A _ := nil\nB _ := undef\n" |> Source.new_source "" |> Lexer.new_lexer
      |> Parser.new_parser
    in
    Parser.run parser;
    let nodes = parser.nodes |> Array.map (fun (n, _) -> n) in

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = A; data_type = `Generics (\"_\"); expr = nil; \
       is_pub = False}"
      (show_ast nodes.(0));

    Alcotest.(check string)
      "same string"
      "Ast.Constant {id = B; data_type = `Generics (\"_\"); expr = undef;\n\
      \  is_pub = False}"
      (show_ast nodes.(1))
end
