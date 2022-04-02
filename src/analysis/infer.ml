open Lily_parser.Ast
module Diagnostic = Lily_lexer.Diagnostic

[@@@warning "-27"]

module IsInt = struct
  open Stdint

  let is8 i = i >= Int128.of_int (-128) && i <= Int128.of_int 127
  let is16 i = i >= Int128.of_int (-32768) && i <= Int128.of_int 32767

  let is32 i =
    i >= Int128.of_int (-2147483648) && i <= Int128.of_int 2147483647

  let is64 i =
    i >= (Int64.min_int |> Int128.of_int64)
    && i <= (Int64.max_int |> Int128.of_int64)

  let is128 i = i >= Int128.min_int && i <= Int128.max_int
end

module IsUint = struct
  open Stdint

  let is8 i = i >= Int128.of_int 0 && i <= Int128.of_int 255
  let is16 i = i >= Int128.of_int 0 && i <= Int128.of_int 65535
  let is32 i = i >= Int128.of_int 0 && i <= Int128.of_int 4294967295

  let is64 i =
    i >= Int128.of_int 0 && i <= (Uint64.max_int |> Int128.of_uint64)

  let is128 i = i >= Stdint.Int128.of_int 0
end

module InferFun = struct
  type t = {
    mutable dt_of_ret : data_type array;
    mutable dt_ret : data_type option;
  }

  let new_t = { dt_of_ret = [||]; dt_ret = None }

  let get_data_type_from_return_arr t typecheck convert arr =
    t.dt_of_ret <-
      Array.map (fun x -> convert typecheck ~specified:None ~neg:false x) arr

  let infer_return_expr t =
    let ret_dt = ref None in
    let rec loop ?(i = 0) () =
      if i < Array.length t.dt_of_ret && Array.length t.dt_of_ret > 1 then (
        let rec loop2 ?(j = i + 1) () =
          if j < Array.length t.dt_of_ret then (
            if t.dt_of_ret.(i) = t.dt_of_ret.(j) then
              ret_dt := Some t.dt_of_ret.(j)
            else if t.dt_of_ret.(j) = `Err then ()
            else failwith "error";
            loop2 ~j:(j + 1) ())
        in
        loop2 ();
        loop ~i:(i + 1) ())
      else if i < Array.length t.dt_of_ret && Array.length t.dt_of_ret = 1
      then ret_dt := Some t.dt_of_ret.(i)
    in
    loop ();
    match t.dt_ret with
    | Some v when Some v = !ret_dt -> v
    | Some v -> failwith "error"
    | None -> ( match !ret_dt with Some v -> v | None -> `Unit)

  let infer_function_type return_expr ~call = assert false
  let infer_arg_type args = assert false
end

let infer_signed_integer_type node ~specified =
  match node with
  | Expr (Literal (Int32 i)), loc -> (
      let i128 = i |> Stdint.Int32.to_int128 in

      let is_i8 = i128 |> IsInt.is8 in
      let is_i16 = i128 |> IsInt.is16 in

      match specified with
      | Some `I8 when is_i8 -> `I8
      | Some `I16 when is_i16 -> `I16
      | Some `I32 -> `I32
      | Some `I64 -> `I64
      | Some `I128 -> `I128
      | None | _ -> failwith "unreachable")
  | Expr (Literal (Int64 i)), loc -> (
      match specified with
      | Some `I64 -> `I64
      | Some `I128 -> `I128
      | None | _ -> failwith "unreachable")
  | Expr (Literal (Int128 i)), loc -> (
      match specified with
      | Some `I128 -> `I128
      | None | _ -> failwith "unreachable")
  | _ -> failwith "unreachable"

let infer_unsigned_integer_type node ~specified =
  match node with
  | Expr (Literal (Int32 i)), loc -> (
      let i128 = i |> Stdint.Int32.to_int128 in

      let is_u8 = i128 |> IsUint.is8 in
      let is_u16 = i128 |> IsUint.is16 in
      let is_u32 = i128 |> IsUint.is32 in
      let is_u64 = i128 |> IsUint.is64 in
      let is_u128 = i128 |> IsUint.is128 in

      match specified with
      | Some `U8 when is_u8 -> `U8
      | Some `U16 when is_u16 -> `U16
      | Some `U32 when is_u32 -> `U32
      | Some `U64 when is_u64 -> `U64
      | Some `U128 when is_u128 -> `U128
      | Some `U8 ->
          Diagnostic.EmitDiagnostic
            ("literal out of range for `Uint8`", Diagnostic.Error, loc)
          |> raise
      | Some `U16 ->
          Diagnostic.EmitDiagnostic
            ("literal out of range for `Uint16`", Diagnostic.Error, loc)
          |> raise
      | Some `U32 ->
          Diagnostic.EmitDiagnostic
            ("literal out of range for `Uint32`", Diagnostic.Error, loc)
          |> raise
      | Some `U64 ->
          Diagnostic.EmitDiagnostic
            ("literal out of range for `Uint64`", Diagnostic.Error, loc)
          |> raise
      | Some `U128 ->
          Diagnostic.EmitDiagnostic
            ("literal out of range for `Uint128`", Diagnostic.Error, loc)
          |> raise
      | None | _ -> failwith "unreachable")
  | Expr (Literal (Int64 i)), loc -> (
      let i128 = i |> Stdint.Int64.to_int128 in

      let is_u64 = IsUint.is64 i128 in
      let is_u128 = IsUint.is128 i128 in

      match specified with
      | Some `U64 when is_u64 -> `U64
      | Some `U128 when is_u128 -> `U128
      | None | _ -> failwith "unreachable")
  | Expr (Literal (Int128 i)), loc -> (
      let is_u128 = IsUint.is128 i in

      match specified with
      | Some `U128 when is_u128 -> `U128
      | None | _ -> failwith "unreachable")
  | _ -> failwith "unreachable"

let infer_integer_type node ~specified ~neg =
  match (specified, neg) with
  | Some `U8, true
  | Some `U16, true
  | Some `U32, true
  | Some `U64, true
  | Some `U128, true ->
      failwith "error"
  | _ -> (
      match specified with
      | Some `U8 | Some `U16 | Some `U32 | Some `U64 | Some `U128 ->
          infer_unsigned_integer_type node ~specified
      | Some `I8 | Some `I16 | Some `I32 | Some `I64 | Some `I128 ->
          infer_signed_integer_type node ~specified
      | None -> (
          match node with
          | Expr (Literal (Int32 _)), _ -> `I32
          | Expr (Literal (Int64 _)), _ -> `I64
          | Expr (Literal (Int128 _)), _ -> `I128
          | _ -> failwith "unreachable")
      | Some t ->
          Diagnostic.EmitDiagnostic
            ( t |> show_data_type
              |> Printf.sprintf "invalid specified type `%s`",
              Diagnostic.Error,
              match node with _, l -> l )
          |> raise)

let infer_float_type loc ~specified =
  match specified with
  | Some `F32 -> `F32
  | Some `F64 -> `F64
  | None -> `F32
  | Some t ->
      Diagnostic.EmitDiagnostic
        ( t |> show_data_type |> Printf.sprintf "invalid specified type `%s`",
          Diagnostic.Error,
          loc )
      |> raise

let infer_tuple_type node = assert false
let infer_array_type node = assert false
let infer_function_type node ~call = assert false
let infer_variable_type node ~call = assert false
let infer_method_type node ~call = assert false
let infer_record_type node ~call = assert false
let infer_expr_type node = assert false
let infer_type node = assert false
