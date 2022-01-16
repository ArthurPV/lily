type location = {
  mutable line : int;
  mutable col : int;
  mutable s_line : int;
  mutable e_line : int;
  mutable s_col : int;
  mutable e_col : int;
}

let new_location =
  { line = 1; col = 1; s_line = 1; e_line = 1; s_col = 1; e_col = 1 }

let copy_location loc = {
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
