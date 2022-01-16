module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
open Lily_lexer.Token

let test () =
  let lexer =
    "** this is a comment\n(* this is a comment multi line *)\n*** this is a comment doc\n"
    |> Source.new_source "" |> Lexer.new_lexer
  in
  Lexer.run lexer;
  let tokens = lexer.tokens |> Array.map (fun (t, _) -> t) in
  
  Alcotest.(check string) "same tokens" "One" (show_token tokens.(0));
  Alcotest.(check string) "same tokens" "Multi" (show_token tokens.(1));
  Alcotest.(check string) "same tokens" "Doc: this is a comment doc" (show_token tokens.(2));
