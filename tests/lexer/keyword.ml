module Lexer = Lily_lexer.Lexer
module Source = Lily_lexer.Source
open Lily_lexer.Token

let test () =
  let lexer =
    "pub self virtual break next if elif else match and or not while for \
     new undef alias record trait enum fun end in import class try catch \
     type object async await module as do include macro test True False \
     return nil mut\n" |> Source.new_source ""
    |> Lexer.new_lexer
  in
  Lexer.run lexer;
  let tokens = lexer.tokens |> Array.map (fun (t, _) -> t) in

  Alcotest.(check string) "same token" "pub" (show_token tokens.(0));
  Alcotest.(check string) "same token" "self" (show_token tokens.(1));
  Alcotest.(check string) "same token" "virtual" (show_token tokens.(2));
  Alcotest.(check string) "same token" "break" (show_token tokens.(3));
  Alcotest.(check string) "same token" "next" (show_token tokens.(4));
  Alcotest.(check string) "same token" "if" (show_token tokens.(5));
  Alcotest.(check string) "same token" "elif" (show_token tokens.(6));
  Alcotest.(check string) "same token" "else" (show_token tokens.(7));
  Alcotest.(check string) "same token" "match" (show_token tokens.(8));
  Alcotest.(check string) "same token" "and" (show_token tokens.(9));
  Alcotest.(check string) "same token" "or" (show_token tokens.(10));
  Alcotest.(check string) "same token" "not" (show_token tokens.(11));
  Alcotest.(check string) "same token" "while" (show_token tokens.(12));
  Alcotest.(check string) "same token" "for" (show_token tokens.(13));
  Alcotest.(check string) "same token" "new" (show_token tokens.(14));
  Alcotest.(check string) "same token" "undef" (show_token tokens.(15));
  Alcotest.(check string) "same token" "alias" (show_token tokens.(16));
  Alcotest.(check string) "same token" "record" (show_token tokens.(17));
  Alcotest.(check string) "same token" "trait" (show_token tokens.(18));
  Alcotest.(check string) "same token" "enum" (show_token tokens.(19));
  Alcotest.(check string) "same token" "fun" (show_token tokens.(20));
  Alcotest.(check string) "same token" "end" (show_token tokens.(21));
  Alcotest.(check string) "same token" "in" (show_token tokens.(22));
  Alcotest.(check string) "same token" "import" (show_token tokens.(23));
  Alcotest.(check string) "same token" "class" (show_token tokens.(24));
  Alcotest.(check string) "same token" "try" (show_token tokens.(25));
  Alcotest.(check string) "same token" "catch" (show_token tokens.(26));
  Alcotest.(check string) "same token" "type" (show_token tokens.(27));
  Alcotest.(check string) "same token" "object" (show_token tokens.(28));
  Alcotest.(check string) "same token" "async" (show_token tokens.(29));
  Alcotest.(check string) "same token" "await" (show_token tokens.(30));
  Alcotest.(check string) "same token" "module" (show_token tokens.(31));
  Alcotest.(check string) "same token" "as" (show_token tokens.(32));
  Alcotest.(check string) "same token" "do" (show_token tokens.(33));
  Alcotest.(check string) "same token" "include" (show_token tokens.(34));
  Alcotest.(check string) "same token" "macro" (show_token tokens.(35));
  Alcotest.(check string) "same token" "test" (show_token tokens.(36));
  Alcotest.(check string) "same token" "True" (show_token tokens.(37));
  Alcotest.(check string) "same token" "False" (show_token tokens.(38));
  Alcotest.(check string) "same token" "return" (show_token tokens.(39));
  Alcotest.(check string) "same token" "nil" (show_token tokens.(40));
  Alcotest.(check string) "same token" "mut" (show_token tokens.(41))
