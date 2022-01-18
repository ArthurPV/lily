module Await = Await
module Const = Constant
module Doc = Doc
module Expr = Expr
module Fun = Fun
module For = For
module If = If
module Import = Import
module LoopStmt = Loop_stmt
module Match = Match
module Object = Object
module Pub = Pub
module Return = Return
module Try = Try
module Type = Type
module While = While

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "expr",
        [
          test_case "Binary exprs test" `Quick Expr.BinaryExpr.test;
          test_case "Complex binary exprs test" `Quick
            Expr.BinaryExpr.complex_test;
          test_case "Call exprs test" `Quick Expr.CallExpr.test;
          test_case "Anonymous fun test" `Quick Expr.AnonymousFunExpr.test;
          test_case "Identifiers test" `Quick Expr.IdentifierExpr.test;
          test_case "Tuple test" `Quick Expr.TupleExpr.test;
          test_case "Array test" `Quick Expr.ArrayExpr.test;
          test_case "Variant test" `Quick Expr.VariantExpr.test;
          test_case "Literals test" `Quick Expr.LiteralExpr.test;
          test_case "Others test" `Quick Expr.OtherExpr.test;
        ] );
      ( "decl",
        [
          test_case "Fun test" `Quick Fun.test;
          test_case "Variable test" `Quick Variable.test;
          test_case "Constant test" `Quick Const.test;
          test_case "Module test" `Quick Module.test;
          test_case "Alias test" `Quick Type.Alias.test;
          test_case "Record test" `Quick Type.Record.test;
          test_case "Enum test" `Quick Type.Enum.test;
          test_case "Class test" `Quick Object.Class.test;
          test_case "Trait test" `Quick Object.Trait.test;
          test_case "Method test" `Quick Object.Method.test;
          test_case "Property test" `Quick Object.Property.test;
          test_case "Pub test" `Quick Pub.test;
          test_case "Import test" `Quick Import.test;
        ] );
      ( "stmt",
        [
          test_case "Return test" `Quick Return.test;
          test_case "If test" `Quick If.test;
          test_case "Await test" `Quick Await.test;
          test_case "Try test" `Quick Try.test;
          test_case "Match test" `Quick Match.test;
          test_case "While test" `Quick While.test;
          test_case "For test" `Quick For.test;
          test_case "Loop stmt test" `Quick LoopStmt.test;
        ] );
      ("doc", [ test_case "Doc test" `Quick Doc.test ]);
    ]
