type location = {
  filename : string; [@printer fun fmt f -> fprintf fmt "%s" f]
  mutable line : int; [@printer fun fmt l -> fprintf fmt "%d" l]
  mutable col : int; [@printer fun fmt c -> fprintf fmt "%d" c]
  mutable s_line : int; [@printer fun fmt sl -> fprintf fmt "%d" sl]
  mutable e_line : int; [@printer fun fmt el -> fprintf fmt "%d" el]
  mutable s_col : int; [@printer fun fmt sc -> fprintf fmt "%d" sc]
  mutable e_col : int; [@printer fun fmt ec -> fprintf fmt "%d" ec]
}
[@@deriving show]

let new_location filename =
  {
    filename;
    line = 1;
    col = 1;
    s_line = 1;
    e_line = 1;
    s_col = 1;
    e_col = 1;
  }

let copy_location loc =
  {
    filename = loc.filename;
    line = loc.line;
    col = loc.col;
    s_line = loc.s_line;
    s_col = loc.s_col;
    e_line = loc.e_line;
    e_col = loc.e_col;
  }

let end_location loc loc2 =
  loc.e_line <- loc2.e_line;
  loc.e_col <- loc2.e_col
