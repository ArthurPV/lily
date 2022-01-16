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
  | LoadConstant of Stdint.uint8 [@printer fun fmt n -> fprintf fmt "LoadConstant(%s)" (Stdint.Uint8.to_string n)]
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
  | Return of Stdint.uint8 [@printer fun fmt n -> fprintf fmt "Return(%s)" (Stdint.Uint8.to_string n)]
[@@deriving show]

val opcode_to_u8 : opcode -> Stdint.uint8
