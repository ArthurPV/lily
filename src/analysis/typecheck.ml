open Lily_parser.Ast

[@@@warning "-27"]

let can_check_type node = assert false

let check_generics_type node = assert false

let check_expr_type node = assert false

let check_type = function
  | Decl (_) -> ()
  | _ -> ()