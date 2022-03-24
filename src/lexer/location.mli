type location = {
  filename: string; [@printer fun fmt f -> fprintf fmt "%s" f]
  mutable line : int; [@printer fun fmt l -> fprintf fmt "%d" l]
  mutable col : int; [@printer fun fmt c -> fprintf fmt "%d" c]
  mutable s_line : int; [@printer fun fmt sl -> fprintf fmt "%d" sl]
  mutable e_line : int; [@printer fun fmt el -> fprintf fmt "%d" el]
  mutable s_col : int; [@printer fun fmt sc -> fprintf fmt "%d" sc]
  mutable e_col : int; [@printer fun fmt ec -> fprintf fmt "%d" ec]
}
[@@deriving show]

val new_location : string -> location
val copy_location : location -> location
val end_location : location -> location -> unit
