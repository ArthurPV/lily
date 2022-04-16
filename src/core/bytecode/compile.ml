module Ast = Lily_parser.Ast
module Opcode = Opcode
module Mod = Lily_common.Mod
(* open Lily_lexer.Location open Opcode open Stack *)

[@@@warning "-27"]
[@@@warning "-26"]

let rec compile_expr ~dt node codes =
  match node with
  | Some (Ast.Add (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Add ]
  | Some (Ast.Sub (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Sub ]
  | Some (Ast.Mul (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Mul ]
  | Some (Ast.Div (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Div ]
  | Some (Ast.Exp (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Exp ]
  | Some (Ast.Mod (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Mod ]
  | Some (Ast.Lt (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Lt ]
  | Some (Ast.Gt (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Gt ]
  | Some (Ast.Le (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Le ]
  | Some (Ast.Ge (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Ge ]
  | Some (Ast.Eq (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Eq ]
  | Some (Ast.Ne (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Ne ]
  | Some (Ast.And (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.And ]
  | Some (Ast.Or (x, y), l) ->
      let lhs = compile_expr ~dt (Some (x, l)) [] in
      let rhs = compile_expr ~dt (Some (y, l)) [] in
      rhs @ lhs @ [ Opcode.Or ]
  | Some (Ast.AddAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.AddTo id; Opcode.StoreVariable id ]
  | Some (Ast.SubAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.SubTo id ]
  | Some (Ast.DivAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.DivTo id ]
  | Some (Ast.MulAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.MulTo id ]
  | Some (Ast.ExpAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.ExpTo id ]
  | Some (Ast.ModAssign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.ModTo id ]
  | Some (Ast.Assign (x, y), l) ->
      let id = get_string_id_from_identifier x in
      let rhs = compile_expr ~dt (Some (x, l)) [] in
      rhs @ [ Opcode.To id ]
  | Some (Ast.Identifier (id, _), _) -> [ Opcode.LoadVariable id ]
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
      compile_expr ~dt None
        (compile_float ~dt (Ast.Literal (Float f)) :: codes)
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

and compile_float ~dt = function
  | Ast.Literal (Float f) -> (
      match dt with
      | Some `F32 -> Opcode.LoadConstant (Float32 f)
      | Some `F64 -> Opcode.LoadConstant (Float64 f)
      | _ -> failwith "unreachable")
  | _ -> failwith "unreachable"

and get_string_id_from_identifier = function
  | Ast.Identifier (id, _) -> id
  | Ast.IdentifierAccess (id, _) | Ast.SelfAccess (id, _) ->
      id
      |> Array.map (fun x -> Ast.show_expr x)
      |> Array.to_list |> String.concat "."
  | _ -> failwith "unreachable"

and can_recude_expression = function
  | Ast.Add (Literal _, Literal _)
  | Ast.Sub (Literal _, Literal _)
  | Ast.Mul (Literal _, Literal _)
  | Ast.Div (Literal _, Literal _)
  | Ast.Exp (Literal _, Literal _)
  | Ast.Mod (Literal _, Literal _)
  | Ast.And (Literal _, Literal _)
  | Ast.Or (Literal _, Literal _)
  | Ast.Negative _ | Ast.Positive _ ->
      true
  | _ -> false

and reduce_expression ~dt = function
  | Ast.Positive (Literal (Int32 x)) ->
      Ast.Literal (Int32 x) |> compile_integer ~dt
  | Ast.Positive (Literal (Int64 x)) ->
      Ast.Literal (Int64 x) |> compile_integer ~dt
  | Ast.Positive (Literal (Int128 x)) ->
      Ast.Literal (Int128 x) |> compile_integer ~dt
  | Ast.Positive (Literal (Float x)) ->
      Ast.Literal (Float x) |> compile_integer ~dt
  | Ast.Negative (Literal (Int32 x)) ->
      Ast.Literal (Int32 (Stdint.Int32.neg x)) |> compile_integer ~dt
  | Ast.Negative (Literal (Int64 x)) ->
      Ast.Literal (Int64 (Stdint.Int64.neg x)) |> compile_integer ~dt
  | Ast.Negative (Literal (Int128 x)) ->
      Ast.Literal (Int128 (Stdint.Int128.neg x)) |> compile_integer ~dt
  | Ast.Negative (Literal (Float x)) ->
      Ast.Literal (Float (Float.neg x)) |> compile_float ~dt
  | Ast.Add (Literal (Int32 x), Literal (Int32 y)) ->
      Ast.Literal (Int32 (Stdint.Int32.( + ) x y)) |> compile_integer ~dt
  | Ast.Add (Literal (Int64 x), Literal (Int64 y)) ->
      Ast.Literal (Int64 (Stdint.Int64.( + ) x y)) |> compile_integer ~dt
  | Ast.Add (Literal (Int128 x), Literal (Int128 y)) ->
      Ast.Literal (Int128 (Stdint.Int128.( + ) x y)) |> compile_integer ~dt
  | Ast.Add (Literal (Float x), Literal (Float y)) ->
      Ast.Literal (Float (x +. y)) |> compile_float ~dt
  | Ast.Sub (Literal (Int32 x), Literal (Int32 y)) ->
      Ast.Literal (Int32 (Stdint.Int32.( - ) x y)) |> compile_integer ~dt
  | Ast.Sub (Literal (Int64 x), Literal (Int64 y)) ->
      Ast.Literal (Int64 (Stdint.Int64.( - ) x y)) |> compile_integer ~dt
  | Ast.Sub (Literal (Int128 x), Literal (Int128 y)) ->
      Ast.Literal (Int128 (Stdint.Int128.( - ) x y)) |> compile_integer ~dt
  | Ast.Sub (Literal (Float x), Literal (Float y)) ->
      Ast.Literal (Float (x -. y)) |> compile_float ~dt
  | Ast.Mul (Literal (Int32 x), Literal (Int32 y)) ->
      Ast.Literal (Int32 (Stdint.Int32.( * ) x y)) |> compile_integer ~dt
  | Ast.Mul (Literal (Int64 x), Literal (Int64 y)) ->
      Ast.Literal (Int64 (Stdint.Int64.( * ) x y)) |> compile_integer ~dt
  | Ast.Mul (Literal (Int128 x), Literal (Int128 y)) ->
      Ast.Literal (Int128 (Stdint.Int128.( * ) x y)) |> compile_integer ~dt
  | Ast.Mul (Literal (Float x), Literal (Float y)) ->
      Ast.Literal (Float (x *. y)) |> compile_float ~dt
  | Ast.Div (Literal (Int32 x), Literal (Int32 y)) ->
      let f1 = Stdint.Int32.to_float x in
      let f2 = Stdint.Int32.to_float y in
      Ast.Literal (Float (f1 /. f2)) |> compile_float ~dt
  | Ast.Div (Literal (Int64 x), Literal (Int64 y)) ->
      let f1 = Stdint.Int64.to_float x in
      let f2 = Stdint.Int64.to_float y in
      Ast.Literal (Float (f1 /. f2)) |> compile_float ~dt
  | Ast.Div (Literal (Int128 x), Literal (Int128 y)) ->
      let f1 = Stdint.Int128.to_float x in
      let f2 = Stdint.Int128.to_float y in
      Ast.Literal (Float (f1 /. f2)) |> compile_float ~dt
  | Ast.Div (Literal (Float x), Literal (Float y)) ->
      Ast.Literal (Float (x /. y)) |> compile_integer ~dt
  | Ast.Mod (Literal (Int32 x), Literal (Int32 y)) ->
      Ast.Literal (Int32 (Mod.Int32.( mod ) x y)) |> compile_integer ~dt
  | Ast.Mod (Literal (Int64 x), Literal (Int64 y)) ->
      Ast.Literal (Int64 (Mod.Int64.( mod ) x y)) |> compile_integer ~dt
  | Ast.Mod (Literal (Int128 x), Literal (Int128 y)) ->
      Ast.Literal (Int128 (Mod.Int128.( mod ) x y)) |> compile_integer ~dt
  | Ast.Exp (Literal (Float x), Literal (Float y)) ->
      Ast.Literal (Float (Float.pow x y)) |> compile_float ~dt
  | Ast.And (Literal (Bool x), Literal (Bool y)) ->
      let const =
        match x && y with true -> Opcode.True | false -> Opcode.False
      in
      Opcode.LoadConstant const
  | Ast.Or (Literal (Bool x), Literal (Bool y)) ->
      let const =
        match x || y with true -> Opcode.True | false -> Opcode.False
      in
      Opcode.LoadConstant const
  (* | Xor *)
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
