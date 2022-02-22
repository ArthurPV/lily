open Location

type diagnostic_kind =
  | Error [@printer fun fmt _ -> fprintf fmt "\x1b[1m\x1b[31merror\x1b[0m\x1b[0m"]
  | Warning [@printer fun fmt _ -> fprintf fmt "\x1b[1m\x1b[33mwarning\x1b[0m\x1b[0m"]
  | Note [@printer fun fmt _ -> fprintf fmt "\x1b[1m\x1b[36mnote\x1b[0m\x1b[0m"]
  | Internal [@printer fun fmt _ -> fprintf fmt "\x1b[1minternal\x1b[0m"]
[@@deriving show]

exception EmitDiagnostic of string * diagnostic_kind * location

type diagnostic = {
  msg : string;
  kind : diagnostic_kind;
  loc : location;
  filename : string;
}

let new_diagnostic ~msg kind loc ~filename = { msg; kind; loc; filename }

let diagnostic_to_string dgn =
  Printf.sprintf "\x1b[1m%s:%d:%d - %s: %s\x1b[0m\n" dgn.filename dgn.loc.line dgn.loc.col
    (show_diagnostic_kind dgn.kind)
    dgn.msg

[@@@warning "-27"]

let get_line_error dgn = failwith "todo"
let emit_diagnostic dgn = Printf.printf "%s" (diagnostic_to_string dgn)
