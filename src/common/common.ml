let rec list_insert v i = function
  | [] -> [ v ]
  | z :: le as l -> if i = 0 then v :: l else z :: list_insert v (i - 1) le
