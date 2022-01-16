type location = {
  mutable line : int;
  mutable col : int;
  mutable s_line : int;
  mutable e_line : int;
  mutable s_col : int;
  mutable e_col : int;
}

val new_location : location
val copy_location : location -> location
val end_location : location -> location -> unit
