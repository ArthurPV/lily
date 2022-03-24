open Location
open Source

type diagnostic_kind =
  | Error
      [@printer
        fun fmt _ -> fprintf fmt "\x1b[1m\x1b[31merror\x1b[0m\x1b[0m"]
  | Warning
      [@printer
        fun fmt _ -> fprintf fmt "\x1b[1m\x1b[33mwarning\x1b[0m\x1b[0m"]
  | Note
      [@printer fun fmt _ -> fprintf fmt "\x1b[1m\x1b[36mnote\x1b[0m\x1b[0m"]
  | Internal [@printer fun fmt _ -> fprintf fmt "\x1b[1minternal\x1b[0m"]
[@@deriving show]

exception EmitDiagnostic of string * diagnostic_kind * location

type diagnostic = {
  msg : string;
  kind : diagnostic_kind;
  loc : location;
  filename : string;
}

let new_diagnostic ~msg kind loc = { msg; kind; loc; filename = loc.filename }

let diagnostic_to_string dgn =
  Printf.sprintf "\x1b[1m%s:%d:%d -\x1b[0m %s\x1b[1m: %s\x1b[0m\n"
    dgn.filename dgn.loc.line dgn.loc.col
    (show_diagnostic_kind dgn.kind)
    dgn.msg

[@@@warning "-27"]

let get_line_error dgn =
  let content =
    (match read_file dgn.filename with
    | Ok s -> s
    | Error e -> failwith "unreachable")
    |> String.split_on_char '\n'
  in
  let rec loop ?(i = 0) ?(line_err = "") () =
    if i < List.length content && i + 1 = dgn.loc.s_line then
      loop ~line_err:(List.nth content i) ~i:(i + 1) ()
    else if i < List.length content then loop ~i:(i + 1) ~line_err ()
    else line_err
  in
  let line_err = loop () in
  let rec loop2 ?(i = 0) ?(s = "") () =
    if i < String.length line_err && dgn.loc.col <> i + 1 then
      match line_err.[i] with
      | '\t' -> loop2 ~i:(i + 1) ~s:(s ^ "\t") ()
      | '\n' -> loop2 ~i:(i + 1) ~s:(s ^ "\n") ()
      | _ -> loop2 ~i:(i + 1) ~s:(s ^ " ") ()
    else if dgn.loc.col = i + 1 then
      loop2 ~i:(i + 1)
        ~s:
          (s
          ^
          match dgn.kind with
          | Error -> "\x1b[1m\x1b[31m^\x1b[0m\x1b[0m"
          | Warning -> "\x1b[1m\x1b[33m^\x1b[0m\x1b[0m"
          | Note -> "\x1b[1m\x1b[36m^\x1b[0m\x1b[0m"
          | Internal -> "\x1b[1m^\x1b[0m")
        ()
    else s
  in
  line_err ^ "\n" ^ loop2 ()

let emit_diagnostic dgn =
  Printf.printf "%s%s\n" (diagnostic_to_string dgn) (get_line_error dgn)
