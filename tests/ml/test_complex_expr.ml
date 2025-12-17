(* Test: Complex expression *)
(* Expected: 47 *)
(* (3 + 4) * 5 + 12 / 3 - 2 = 35 + 4 - 2 = 37... wait *)
(* Let's do: (3 + 4) * 5 + 12 = 35 + 12 = 47 *)

let main = print_int ((3 + 4) * 5 + 12)
