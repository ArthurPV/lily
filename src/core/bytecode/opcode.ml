type opcode =
  | Noop
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt
  | Gt
  | Le
  | Ge
  | Jump
  | JumpIf
  | Eq
  | Ne
  | LoadNil
  | LoadTrue
  | LoadFalse
  | LoadConstant of Stdint.uint8
      [@printer
        fun fmt n ->
          fprintf fmt "LoadConstant(%s)" (Stdint.Uint8.to_string n)]
  | LoadSelf
  | Closure
  | Push
  | Call
  | In
  | Get
  | Put
  | Length
  | MakeArray
  | MakeString
  | MakeTuple
  | MakeRecord
  | MakeClass
  | MakeFunction
  | Construct
  | And
  | Or
  | Not
  | Return of Stdint.uint8
      [@printer
        fun fmt n -> fprintf fmt "Return(%s)" (Stdint.Uint8.to_string n)]
[@@deriving show]

let opcode_to_u8 = function
  | Noop -> Stdint.Uint8.of_int 0
  | Add -> Stdint.Uint8.of_int 1
  | Sub -> Stdint.Uint8.of_int 2
  | Mul -> Stdint.Uint8.of_int 3
  | Div -> Stdint.Uint8.of_int 4
  | Mod -> Stdint.Uint8.of_int 5
  | Lt -> Stdint.Uint8.of_int 6
  | Gt -> Stdint.Uint8.of_int 7
  | Le -> Stdint.Uint8.of_int 8
  | Ge -> Stdint.Uint8.of_int 9
  | Jump -> Stdint.Uint8.of_int 10
  | JumpIf -> Stdint.Uint8.of_int 11
  | Eq -> Stdint.Uint8.of_int 12
  | Ne -> Stdint.Uint8.of_int 13
  | LoadNil -> Stdint.Uint8.of_int 14
  | LoadTrue -> Stdint.Uint8.of_int 15
  | LoadFalse -> Stdint.Uint8.of_int 16
  | LoadConstant _ -> Stdint.Uint8.of_int 17
  | LoadSelf -> Stdint.Uint8.of_int 18
  | Closure -> Stdint.Uint8.of_int 19
  | Push -> Stdint.Uint8.of_int 20
  | Call -> Stdint.Uint8.of_int 21
  | In -> Stdint.Uint8.of_int 22
  | Get -> Stdint.Uint8.of_int 23
  | Put -> Stdint.Uint8.of_int 24
  | Length -> Stdint.Uint8.of_int 25
  | MakeArray -> Stdint.Uint8.of_int 26
  | MakeString -> Stdint.Uint8.of_int 27
  | MakeTuple -> Stdint.Uint8.of_int 28
  | MakeRecord -> Stdint.Uint8.of_int 29
  | MakeClass -> Stdint.Uint8.of_int 30
  | MakeFunction -> Stdint.Uint8.of_int 31
  | Construct -> Stdint.Uint8.of_int 32
  | And -> Stdint.Uint8.of_int 33
  | Or -> Stdint.Uint8.of_int 34
  | Not -> Stdint.Uint8.of_int 35
  | Return _ -> Stdint.Uint8.of_int 36
