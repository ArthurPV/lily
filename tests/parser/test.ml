module Expr = Expr

(* Run it *)
let () =
  let open Alcotest in
  run "Parser" [
      ("expr", [ test_case "Test binary expr" `Quick Expr.BinaryExpr.test ])
    ]
