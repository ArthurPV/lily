[@@@warning "-27"]

open Argument
open Lily_common.Error
open Lily_command.Run

module BuildOptions = struct
  type path = string
  type t_kind = Help | Input of path | None | Err of cli_error_kind
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }

  let parse arg t =
    let rec loop ?(i = 2) () =
      if i < Array.length arg.argv then
        match arg.argv.(i) with
        | "-h" | "--help" ->
            t.op_arr <- Array.append t.op_arr [| Help |];
            loop ~i:(i + 1) ()
        | "" ->
            t.op_arr <- Array.append t.op_arr [| None |];
            loop ~i:(i + 1) ()
        | path when path.[0] <> '-' ->
            t.op_arr <- Array.append t.op_arr [| Input path |];
            loop ~i:(i + 1) ()
        | e -> t.op_arr <- Array.append t.op_arr [| Err (BadOption e) |]
    in
    loop ()

  let get_error t =
    if Array.length t.op_arr > 0 then
      match t.op_arr.(Array.length t.op_arr - 1) with
      | Err e ->
          e |> show_cli_error_kind |> print_cli_error;
          exit 1
      | _ -> ()
    else ()

  let run ?(i = 0) arg t parse =
    parse arg t;
    get_error t;
    if i < Array.length t.op_arr then
      match t.op_arr.(i) with
      | Help -> Printf.printf "Help"
      | Input p -> Printf.printf "build"
      | None -> ()
      | _ -> failwith "unreachable"
end

module CompileOptions = struct
  type path = string

  type t_kind =
    | Help
    | Input of path
    | None
    | DumpLexer
    | DumpParser
    | DumpAST
    | DumpIR
    | Object

  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }
  let parse arg t = assert false
  let run ?(i = 0) arg t parse = ()
end

module InitOptions = struct
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }
  let parse arg t = assert false
  let run ?(i = 0) arg t parse = ()
end

module NewOptions = struct
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }
  let parse arg t = ()
  let run ?(i = 0) arg t parse = ()
end

module RunOptions = struct
  type path = string
  type t_kind = Help | None | Input of path | Err of cli_error_kind
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }

  let parse arg t =
    let rec loop ?(i = 2) () =
      if i < Array.length arg.argv then
        match arg.argv.(i) with
        | "-h" | "--help" ->
            t.op_arr <- Array.append t.op_arr [| Help |];
            loop ~i:(i + 1) ()
        | "" ->
            t.op_arr <- Array.append t.op_arr [| None |];
            loop ~i:(i + 1) ()
        | path when path.[0] <> '-' ->
            t.op_arr <- Array.append t.op_arr [| Input path |];
            loop ~i:(i + 1) ()
        | e -> t.op_arr <- Array.append t.op_arr [| Err (BadOption e) |]
    in
    loop ()

  let get_error t =
    if Array.length t.op_arr > 0 then
      match t.op_arr.(Array.length t.op_arr - 1) with
      | Err e ->
          e |> show_cli_error_kind |> print_cli_error;
          exit 1
      | _ -> ()
    else ()

  let run ?(i = 0) arg t parse =
    parse arg t;
    get_error t;
    if i < Array.length t.op_arr then
      match t.op_arr.(i) with
      | Help -> Printf.printf "Help"
      | Input p -> run_bytecode p
      | None -> ()
      | _ -> failwith "unreachable"
end

module TestOptions = struct
  type path = string
  type t_kind = Help | None | Input of path
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }
  let parse arg t = assert false
  let run ?(i = 0) arg t parse = ()
end

module ToOptions = struct
  type to_language_kind = Js | Ml | Rb | C
  type path = string
  type t_kind = Help | None | Input of path | Required of to_language_kind
  type t = { mutable op_arr : t_kind array }

  let new_t = { op_arr = [||] }
  let parse arg t = assert false
  let run ?(i = 0) arg t parse = ()
end
