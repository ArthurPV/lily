module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
open Lily_lexer.Token

let test () =
  let lexer =
    "_ac\nz3\nAAA\ne4WWWQ\n"
    |> Source.new_source "" |> Lexer.new_lexer
  in
  Lexer.run lexer;
  let tokens = lexer.tokens |> Array.map (fun (t, _) -> t) in
  
  Alcotest.(check string) "same tokens" "_ac" (show_token tokens.(0));
  Alcotest.(check string) "same tokens" "z3" (show_token tokens.(1));
  Alcotest.(check string) "same tokens" "AAA" (show_token tokens.(2));
  Alcotest.(check string) "same tokens" "e4WWWQ" (show_token tokens.(3));
