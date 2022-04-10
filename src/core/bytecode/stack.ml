type 'a stack = { mutable st : 'a list }

let is_empty stack = stack.st = []
let push stack value = stack.st <- value :: stack.st |> List.rev

let rec pop stack ?(update = []) () =
  match stack.st with
  | [] -> failwith "Impossible to pop from empty stack"
  | [ h ] ->
      stack.st <- update |> List.rev;
      h
  | h :: le ->
      stack.st <- le;
      pop stack ~update:(h :: update) ()
