(* x := 0 -> LoadConstant (LoadInt32 0) StoreVariable x *)

type value =
  | True [@printer fun fmt _ -> fprintf fmt "`True"]
  | False [@printer fun fmt _ -> fprintf fmt "`False"]
  | String of string [@printer fun fmt s -> fprintf fmt "`String(%s)" s]
  | Char of char [@printer fun fmt c -> fprintf fmt "`Char(%c)" c]
  | Int8 of Stdint.int8
      [@printer
        fun fmt i -> fprintf fmt "`Int8(%s)" (Stdint.Int8.to_string i)]
  | Int16 of Stdint.int16
      [@printer
        fun fmt i -> fprintf fmt "`Int16(%s)" (Stdint.Int16.to_string i)]
  | Int32 of Stdint.int32
      [@printer
        fun fmt i -> fprintf fmt "`Int32(%s)" (Stdint.Int32.to_string i)]
  | Int64 of Stdint.int64
      [@printer
        fun fmt i -> fprintf fmt "`Int64(%s)" (Stdint.Int64.to_string i)]
  | Int128 of Stdint.int128
      [@printer
        fun fmt i -> fprintf fmt "`Int128(%s)" (Stdint.Int128.to_string i)]
  | Uint8 of Stdint.uint8
      [@printer
        fun fmt i -> fprintf fmt "`Uint8(%s)" (Stdint.Uint8.to_string i)]
  | Uint16 of Stdint.uint16
      [@printer
        fun fmt i -> fprintf fmt "`Uint16(%s)" (Stdint.Uint16.to_string i)]
  | Uint32 of Stdint.uint32
      [@printer
        fun fmt i -> fprintf fmt "`Uint32(%s)" (Stdint.Uint32.to_string i)]
  | Uint64 of Stdint.uint64
      [@printer
        fun fmt i -> fprintf fmt "`Uint64(%s)" (Stdint.Uint64.to_string i)]
  | Uint128 of Stdint.uint128
      [@printer
        fun fmt i -> fprintf fmt "`Uint128(%s)" (Stdint.Uint128.to_string i)]
  | Float32 of float
      [@printer fun fmt f -> fprintf fmt "`Float32(%s)" (Float.to_string f)]
  | Float64 of float
      [@printer fun fmt f -> fprintf fmt "`Float64(%s)" (Float.to_string f)]
  | Array [@printer fun fmt _ -> fprintf fmt "Array"]
  | Tuple [@printer fun fmt _ -> fprintf fmt "Tuple"]
  | Record [@printer fun fmt _ -> fprintf fmt "Record"]
  | Object [@printer fun fmt _ -> fprintf fmt "Object"]
  | Undef [@printer fun fmt _ -> fprintf fmt "Undef"]
  | Nil [@printer fun fmt _ -> fprintf fmt "Nil"]
[@@deriving show]

type opcode =
  | Noop [@printer fun fmt _ -> fprintf fmt "`Noop"]
  | Add [@printer fun fmt _ -> fprintf fmt "`Add"]
  | Sub [@printer fun fmt _ -> fprintf fmt "`Sub"]
  | Div [@printer fun fmt _ -> fprintf fmt "`Div"]
  | Mul [@printer fun fmt _ -> fprintf fmt "`Mul"]
  | Exp [@printer fun fmt _ -> fprintf fmt "`Exp"]
  | Mod [@printer fun fmt _ -> fprintf fmt "`Mod"]
  | AddTo [@printer fun fmt _ -> fprintf fmt "`AddTo"]
  | SubTo [@printer fun fmt _ -> fprintf fmt "`SubTo"]
  | DivTo [@printer fun fmt _ -> fprintf fmt "`DivTo"]
  | MulTo [@printer fun fmt _ -> fprintf fmt "`MulTo"]
  | ExpTo [@printer fun fmt _ -> fprintf fmt "`ExpTo"]
  | ModTo [@printer fun fmt _ -> fprintf fmt "`ModTo"]
  | To [@printer fun fmt _ -> fprintf fmt "`To"]
  | Lt [@printer fun fmt _ -> fprintf fmt "`Lt"]
  | Gt [@printer fun fmt _ -> fprintf fmt "`Gt"]
  | Le [@printer fun fmt _ -> fprintf fmt "`Le"]
  | Ge [@printer fun fmt _ -> fprintf fmt "`Ge"]
  | Jump [@printer fun fmt _ -> fprintf fmt "`Jump"]
  | JumpIf [@printer fun fmt _ -> fprintf fmt "`JumpIf"]
  | Eq [@printer fun fmt _ -> fprintf fmt "`Eq"]
  | Ne [@printer fun fmt _ -> fprintf fmt "`Ne"]
  | Or [@printer fun fmt _ -> fprintf fmt "`Or"]
  | (* | `Xor *)
    And [@printer fun fmt _ -> fprintf fmt "`And"]
  | LoadConstant of value
      [@printer fun fmt v -> fprintf fmt "`LoadConstant(%s)" (show_value v)]
  | StoreVariable of string
      [@printer fun fmt var -> fprintf fmt "`StoreVariable(%s)" var]
  | StoreFunction of string
      [@printer fun fmt s -> fprintf fmt "`StoreFunction(%s)" s]
  | LoadVariable of string
      [@printer fun fmt s -> fprintf fmt "`LoadVariable(%s)" s]
  | LoadFunction of string
      [@printer fun fmt s -> fprintf fmt "`LoadFunction(%s)" s]
  | Return [@printer fun fmt _ -> fprintf fmt "`Return"]
[@@deriving show]

let opcode_to_u8 = function
  | Noop -> Stdint.Uint8.of_int 0
  | Add -> Stdint.Uint8.of_int 1
  | Sub -> Stdint.Uint8.of_int 2
  | Mul -> Stdint.Uint8.of_int 3
  | Div -> Stdint.Uint8.of_int 4
  | Mod -> Stdint.Uint8.of_int 5
  | Exp -> Stdint.Uint8.of_int 6
  | AddTo -> Stdint.Uint8.of_int 7
  | SubTo -> Stdint.Uint8.of_int 8
  | DivTo -> Stdint.Uint8.of_int 9
  | MulTo -> Stdint.Uint8.of_int 10
  | ExpTo -> Stdint.Uint8.of_int 11
  | ModTo -> Stdint.Uint8.of_int 12
  | To -> Stdint.Uint8.of_int 13
  | Lt -> Stdint.Uint8.of_int 14
  | Gt -> Stdint.Uint8.of_int 15
  | Le -> Stdint.Uint8.of_int 16
  | Ge -> Stdint.Uint8.of_int 17
  | Jump -> Stdint.Uint8.of_int 18
  | JumpIf -> Stdint.Uint8.of_int 19
  | Eq -> Stdint.Uint8.of_int 20
  | Ne -> Stdint.Uint8.of_int 21
  | Or -> Stdint.Uint8.of_int 22
  | And -> Stdint.Uint8.of_int 23
  | LoadConstant _ -> Stdint.Uint8.of_int 24
  | StoreVariable _ -> Stdint.Uint8.of_int 25
  | StoreFunction _ -> Stdint.Uint8.of_int 26
  | LoadVariable _ -> Stdint.Uint8.of_int 27
  | LoadFunction _ -> Stdint.Uint8.of_int 28
  | Return -> Stdint.Uint8.of_int 29
