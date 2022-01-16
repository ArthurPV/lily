module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
open Lily_lexer.Token

let test () =
  let lexer =
    "+ - * / % ^ ++ -- := += -= *= /= %= ^= = == .. < > <= >= != ? &\n"
    |> Source.new_source "" |> Lexer.new_lexer
  in
  Lexer.run lexer;
  let tokens = lexer.tokens |> Array.map (fun (t, _) -> t) in

  Alcotest.(check string) "same token" "+" (show_token tokens.(0));
  Alcotest.(check string) "same token" "-" (show_token tokens.(1));
  Alcotest.(check string) "same token" "*" (show_token tokens.(2));
  Alcotest.(check string) "same token" "/" (show_token tokens.(3));
  Alcotest.(check string) "same token" "%" (show_token tokens.(4));
  Alcotest.(check string) "same token" "^" (show_token tokens.(5));
  Alcotest.(check string) "same token" "++" (show_token tokens.(6));
  Alcotest.(check string) "same token" "--" (show_token tokens.(7));
  Alcotest.(check string) "same token" ":=" (show_token tokens.(8));
  Alcotest.(check string) "same token" "+=" (show_token tokens.(9));
  Alcotest.(check string) "same token" "-=" (show_token tokens.(10));
  Alcotest.(check string) "same token" "*=" (show_token tokens.(11));
  Alcotest.(check string) "same token" "/=" (show_token tokens.(12));
  Alcotest.(check string) "same token" "%=" (show_token tokens.(13));
  Alcotest.(check string) "same token" "^=" (show_token tokens.(14));
  Alcotest.(check string) "same token" "=" (show_token tokens.(15));
  Alcotest.(check string) "same token" "==" (show_token tokens.(16));
  Alcotest.(check string) "same token" ".." (show_token tokens.(17));
  Alcotest.(check string) "same token" "<" (show_token tokens.(18));
  Alcotest.(check string) "same token" ">" (show_token tokens.(19));
  Alcotest.(check string) "same token" "<=" (show_token tokens.(20));
  Alcotest.(check string) "same token" ">=" (show_token tokens.(21));
  Alcotest.(check string) "same token" "!=" (show_token tokens.(22));
  Alcotest.(check string) "same token" "?" (show_token tokens.(23));
  Alcotest.(check string) "same token" "&" (show_token tokens.(24))
