module Int32 = struct
  let ( mod ) x y =
    let rec loop ?(i = Stdint.Int32.of_int 0) () =
      if Stdint.Int32.( * ) i y <= x then
        loop ~i:(Stdint.Int32.of_int 1 |> Stdint.Int32.( + ) i) ()
      else Stdint.Int32.( * ) i y
    in
    loop ()
end

module Int64 = struct
  let ( mod ) x y =
    let rec loop ?(i = Stdint.Int64.of_int 0) () =
      if Stdint.Int64.( * ) i y <= x then
        loop ~i:(Stdint.Int64.of_int 1 |> Stdint.Int64.( + ) i) ()
      else Stdint.Int64.( * ) i y
    in
    loop ()
end

module Int128 = struct
  let ( mod ) x y =
    let rec loop ?(i = Stdint.Int128.of_int 0) () =
      if Stdint.Int128.( * ) i y <= x then
        loop ~i:(Stdint.Int128.of_int 1 |> Stdint.Int128.( + ) i) ()
      else Stdint.Int128.( * ) i y
    in
    loop ()
end
