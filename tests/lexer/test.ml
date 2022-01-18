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
      ("comment", [ test_case "Comments test" `Quick Comment.test ]);
      ("identifier", [ test_case "Identifiers test" `Quick Identifier.test ]);
      ("keyword", [ test_case "Keywords test" `Quick Keyword.test ]);
      ("literal", [ test_case "Literals test" `Quick Literal.test ]);
      ("operator", [ test_case "Operators test" `Quick Operator.test ]);
      ("separator", [ test_case "Separators test" `Quick Separator.test ]);
    ]
