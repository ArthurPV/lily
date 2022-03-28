open Lily_parser.Ast

[@@@warning "-27"]

module IsInt = struct
  open Stdint

  let is8 i = i >= Int128.of_int (-128) && i <= Int128.of_int 127
  let is16 i = i >= Int128.of_int (-32768) && i <= Int128.of_int 32767

  let is32 i =
    i >= Int128.of_int (-2147483648) && i <= Int128.of_int 2147483647

  let is64 i = i >= Int128.min_int && i <= Int128.max_int
end

module IsUint = struct
  open Stdint

  let is8 i = i >= Int128.of_int 0 && i <= Int128.of_int 255
  let is16 i = i >= Int128.of_int 0 && i <= Int128.of_int 65535
  let is32 i = i >= Int128.of_int 0 && i <= Int128.of_int 4294967295
  let is64 i = i >= Int128.of_int 0 && i <= Int128.max_int
end

module InferFun = struct
  type t = {
    mutable dt_of_ret : data_type array;
    mutable dt_ret : data_type option;
  }

  let new_t = { dt_of_ret = [||]; dt_ret = None }

  let get_data_type_from_return_arr t typecheck convert arr =
    t.dt_of_ret <- Array.map (fun x -> convert typecheck x) arr

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

let infer_integer_type node =
  match node with
  | Expr (Literal (Int32 _)), _ -> `I32
  | Expr (Literal (Int64 _)), _ -> `I64
  | Expr (Literal (Int128 _)), _ -> `I128
  | _ -> failwith "unreachable"

let infer_float_type node = assert false
let infer_tuple_type node = assert false
let infer_array_type node = assert false
let infer_function_type node ~call = assert false
let infer_variable_type node ~call = assert false
let infer_method_type node ~call = assert false
let infer_record_type node ~call = assert false
let infer_expr_type node = assert false
let infer_type node = assert false
