open Lily_parser.Ast
open Lily_analysis.Scope

module LIR : sig
  type t =
    | Int of Stdint.int128
        [@printer
          fun fmt i -> fprintf fmt "Int(%s)" (Stdint.Int128.to_string i)]
    | Float of float [@printer fun fmt f -> fprintf fmt "Float(%f)" f]
    | Bool of bool
        [@printer
          fun fmt b -> fprintf fmt "Bool(%s)" (if b then "True" else "False")]
    | String of string [@printer fun fmt s -> fprintf fmt "String(%s)" s]
    | Char of char [@printer fun fmt c -> fprintf fmt "Char(%c)" c]
    | Unit [@printer fun fmt _ -> fprintf fmt "Unit"]
    | Object [@printer fun fmt _ -> fprintf fmt "Object"]
    | Array of t array
        [@printer
          fun fmt arr ->
            let rec loop ?(i = 0) ?(l = []) () =
              if i < Array.length arr then
                loop ~i:(i + 1) ~l:(show arr.(i) :: l) ()
              else String.concat ", " l
            in
            fprintf fmt "Array(%s)" (loop ())]
    | Tuple of t array
    | Fun of string * t array (* TODO: add args *)
    | Class of string * t array
    | Method of string * t array
    | Variable of string * t
    | Constant of string * t
    | Block of t option * t array
    | Call of t array * t
    | Ret of t [@printer fun fmt v -> fprintf fmt "Ret(%s)" (show v)]
    | Undef [@printer fun fmt _ -> fprintf fmt "Undef"]
    | Nil [@printer fun fmt _ -> fprintf fmt "Nil"]
  [@@deriving show]

  val to_int : t -> Stdint.int128
  val to_float : t -> float
  val to_bool : t -> bool
  val to_string : t -> string
  val to_char : t -> char
end

type compiler = { scope : scope; nodes_value : LIR.t array }

val compile : ast -> LIR.t
val run : compiler -> unit
