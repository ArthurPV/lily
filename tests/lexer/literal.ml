module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
open Lily_lexer.Token

let test () =
  let lexer =
    "\"Hello\"\n\'c\'\n3.3333\n2e+3\n0xff\n0b0101\n0o2207\n333\n"
    |> Source.new_source "" |> Lexer.new_lexer
  in
  Lexer.run lexer;
  let tokens = lexer.tokens |> Array.map (fun (t, _) -> t) in

  Alcotest.(check string) "same token" "\"Hello\"" (show_token tokens.(0));
  Alcotest.(check string) "same token" "\'c\'" (show_token tokens.(1));
  Alcotest.(check string) "same token" "3.3333" (show_token tokens.(2));
  Alcotest.(check string) "same token" "2e+3" (show_token tokens.(3));
  Alcotest.(check string) "same token" "0xff" (show_token tokens.(4));
  Alcotest.(check string) "same token" "0b0101" (show_token tokens.(5));
  Alcotest.(check string) "same token" "0o2207" (show_token tokens.(6));
  Alcotest.(check string) "same token" "333" (show_token tokens.(7))
