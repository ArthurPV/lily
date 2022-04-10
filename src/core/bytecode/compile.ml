module Ast = Lily_parser.Ast
module Opcode = Opcode
(* open Lily_lexer.Location open Opcode open Stack *)

[@@@warning "-27"]
[@@@warning "-26"]

let rec compile_expr ~dt node codes =
  match node with
  | Some (Ast.Add (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      if get_precedence (Ast.Add (x, y)) < get_precedence y then
        rhs @ lhs @ [ Opcode.Add ]
      else rhs @ lhs @ [ Opcode.Add ]
  | Some (Ast.Sub (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      if get_precedence (Ast.Sub (x, y)) < get_precedence y then
        rhs @ lhs @ [ Opcode.Sub ]
      else rhs @ lhs @ [ Opcode.Sub ]
  | Some (Ast.Mul (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      if get_precedence (Ast.Mul (x, y)) < get_precedence y then
        rhs @ lhs @ [ Opcode.Mul ]
      else rhs @ lhs @ [ Opcode.Mul ]
  | Some (Ast.Div (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      if get_precedence (Ast.Div (x, y)) < get_precedence y then
        rhs @ lhs @ [ Opcode.Div ]
      else rhs @ lhs @ [ Opcode.Mul ]
  | Some (Ast.Exp (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      if get_precedence (Ast.Exp (x, y)) < get_precedence y then
        rhs @ lhs @ [ Opcode.Exp ]
      else rhs @ lhs @ [ Opcode.Exp ]
  | Some (Ast.Literal (Int32 i), _) ->
      compile_expr ~dt None
        (compile_integer ~dt (Ast.Literal (Int32 i)) :: codes)
  | Some (Ast.Literal (Ast.Int64 i), _) ->
      compile_expr ~dt None
        (compile_integer ~dt (Ast.Literal (Int64 i)) :: codes)
  | Some (Ast.Literal (Int128 i), _) ->
      compile_expr ~dt None
        (compile_integer ~dt (Ast.Literal (Int128 i)) :: codes)
  | Some (Ast.Literal (Float f), _) ->
      let op =
        match dt with
        | Some `F32 -> Opcode.LoadConstant (Float32 f)
        | Some `F64 -> Opcode.LoadConstant (Float64 f)
        | _ -> failwith "unreachable"
      in
      compile_expr ~dt None (op :: codes)
  | Some (Ast.Literal (Bool true), _) ->
      compile_expr ~dt None (Opcode.LoadConstant True :: codes)
  | Some (Ast.Literal (Bool false), _) ->
      compile_expr ~dt None (Opcode.LoadConstant False :: codes)
  | Some (Ast.Literal (Char c), _) ->
      compile_expr ~dt None (Opcode.LoadConstant (Char c) :: codes)
  | Some (Ast.Literal (String s), _) ->
      compile_expr ~dt None (Opcode.LoadConstant (String s) :: codes)
  | None -> codes
  | _ -> failwith "todo"

and compile_integer ~dt = function
  | Literal (Int32 i) -> (
      match dt with
      | Some `I8 -> Opcode.LoadConstant (Int8 (Stdint.Int8.of_int32 i))
      | Some `I16 -> Opcode.LoadConstant (Int16 (Stdint.Int16.of_int32 i))
      | Some `I32 -> Opcode.LoadConstant (Int32 i)
      | Some `I64 -> Opcode.LoadConstant (Int64 (Stdint.Int64.of_int32 i))
      | Some `I128 -> Opcode.LoadConstant (Int128 (Stdint.Int128.of_int32 i))
      | Some `U8 -> Opcode.LoadConstant (Uint8 (Stdint.Uint8.of_int32 i))
      | Some `U16 -> Opcode.LoadConstant (Uint16 (Stdint.Uint16.of_int32 i))
      | Some `U32 -> Opcode.LoadConstant (Uint32 (Stdint.Uint32.of_int32 i))
      | Some `U64 -> Opcode.LoadConstant (Uint64 (Stdint.Uint64.of_int32 i))
      | Some `U128 ->
          Opcode.LoadConstant (Uint128 (Stdint.Uint128.of_int32 i))
      | _ -> failwith "unreachable")
  | Literal (Int64 i) -> (
      match dt with
      | Some `I64 -> Opcode.LoadConstant (Int64 i)
      | Some `I128 -> Opcode.LoadConstant (Int128 (Stdint.Int128.of_int64 i))
      | Some `U64 -> Opcode.LoadConstant (Uint64 (Stdint.Uint64.of_int64 i))
      | Some `U128 ->
          Opcode.LoadConstant (Uint128 (Stdint.Uint128.of_int64 i))
      | _ -> failwith "unreachable")
  | Literal (Int128 i) -> (
      match dt with
      | Some `I128 -> Opcode.LoadConstant (Int128 i)
      | Some `U128 ->
          Opcode.LoadConstant (Uint128 (Stdint.Uint128.of_int128 i))
      | _ -> failwith "unreachable")
  | _ -> failwith "unreachable"

and get_precedence = function
  | Ast.AddAssign _ | Ast.SubAssign _ | Ast.MulAssign _ | Ast.DivAssign _
  | Ast.Assign _ | Ast.ExpAssign _ | Ast.ModAssign _ | Ast.Literal _ ->
      0
  | Ast.Or _ -> 1
  | Ast.And _ -> 2
  | Ast.Eq _ | Ast.Ne _ -> 3
  | Ast.Lt _ | Ast.Gt _ | Ast.Le _ | Ast.Ge _ -> 4
  | Ast.Range _ -> 5
  | Ast.Add _ | Ast.Sub _ -> 6
  | Ast.Mul _ | Ast.Div _ | Ast.Mod _ -> 7
  | Ast.Exp _ -> 8
  | Ast.Positive _ | Ast.Negative _ | Ast.Not _ -> 9
  | _ -> failwith "unreachable"

and compile_variable node =
  match node with
  | Ast.Decl (Variable { id; expr; data_type = dt; _ }), l ->
      let l = compile_expr ~dt (Some (expr, l)) [] in
      l @ [ StoreVariable id ]
  | _ -> failwith "unreachable"

and compile_function node = assert false

and run node =
  match node with
  | Ast.Decl (Fun { body; _ }), _ ->
      let rec loop ?(codes = []) ?(i = 0) () =
        if i < Array.length body then
          match body.(i) with
          | (Decl (Variable _), _) as var ->
              loop ~codes:(codes @ compile_variable var) ~i:(i + 1) ()
          | _ -> failwith "unreachable"
        else codes
      in
      loop ()
  | _ -> failwith "unreachable"
