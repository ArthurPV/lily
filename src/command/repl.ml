type repl = string -> int -> int

external new_repl : unit -> repl = "new_repl"
external run_repl : repl -> unit = "run"
