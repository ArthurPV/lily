type 'a stack = { mutable st : 'a list }

val is_empty : 'a stack -> bool
val push : 'a stack -> 'a -> unit
val pop : 'a stack -> ?update:'a list -> unit -> 'a
