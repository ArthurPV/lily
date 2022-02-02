type cli_error_kind =
  | UnexpectedArgument of string
      [@printer fun fmt arg -> fprintf fmt "unexpected argument: `%s`" arg]
  | FileNotExists of string
      [@printer
        fun fmt filename -> fprintf fmt "the file not exists: `%s`" filename]
  | FileIsDirectory of string
      [@printer
        fun fmt filename ->
          fprintf fmt "the file is directory: `%s`" filename]
  | FileHasBadExtension of string
      [@printer
        fun fmt filename ->
          fprintf fmt "the file has bad extension: `%s`" filename]
  | FileIsNotSpecified
      [@printer fun fmt _ -> fprintf fmt "the file is not specified"]
  | ReplDocArgumentFailed of string
      [@printer
        fun fmt arg -> fprintf fmt "the repl doc argument failed: `%s`" arg]
  | UnknownCommand of string
      [@printer fun fmt c -> fprintf fmt "unknown command: `%s`" c]
  | BadOption of string
      [@printer fun fmt op -> fprintf fmt "bad option: `%s`" op]
[@@deriving show]

let process_error msg =
  Printf.printf "error: %s\n" msg;
  exit 1

let print_cli_error msg = Printf.printf "error: %s\n" msg
