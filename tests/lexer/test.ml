module Comment = Comment
module Identifier = Identifier
module Keyword = Keyword
module Literal = Literal
module Operator = Operator
module Separator = Separator

let () =
  let open Alcotest in
  run "Lexer"
    [
      ("comment", [ test_case "Test comment" `Quick Comment.test ]);
      ("identifier", [ test_case "Test identifier" `Quick Identifier.test ]);
      ("keyword", [ test_case "Test keyword" `Quick Keyword.test ]);
      ("literal", [ test_case "Test literal" `Quick Literal.test ]);
      ("operator", [ test_case "Test operator" `Quick Operator.test ]);
      ("separator", [ test_case "Test separator" `Quick Separator.test ]);
    ]
