open Location

type diagnostic_kind =
  | Error [@printer fun fmt _ -> fprintf fmt "error"]
  | Warning [@printer fun fmt _ -> fprintf fmt "warning"]
  | Note [@printer fun fmt _ -> fprintf fmt "note"]
  | Internal [@printer fun fmt _ -> fprintf fmt "internal"]
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
  Printf.sprintf "%s:%d:%d - %s: %s\n" dgn.filename dgn.loc.line dgn.loc.col
    (show_diagnostic_kind dgn.kind)
    dgn.msg

[@@@warning "-27"]

let get_line_error dgn = failwith "todo"
let emit_diagnostic dgn = Printf.printf "%s" (diagnostic_to_string dgn)
