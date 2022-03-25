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

val new_diagnostic : msg:string -> diagnostic_kind -> location -> diagnostic
val diagnostic_to_string : diagnostic -> string
val get_line_error : diagnostic -> string
val emit_diagnostic : diagnostic -> unit
