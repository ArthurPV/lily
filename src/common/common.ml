let rec list_insert v i = function
  | [] -> [ v ]
  | z :: le as l -> if i = 0 then v :: l else z :: list_insert v (i - 1) le

let rec list_remove n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: list_remove (n - 1) t

let list_remove_last = function
  | [] -> []
  | t ->
      Array.sub (t |> Array.of_list) 0 (List.length t - 1) |> Array.to_list
