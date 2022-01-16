module Run = Lily_command.Run
open Argument

let () =
  let arg = new_argument in
  if arg.argc > 1 then
    Run.run_bytecode arg.argv.(2)
  else
    Printf.printf "Hello"
