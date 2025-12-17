(* Test: Max function *)
(* Expected: 42 *)

let max a b = if a > b then a else b
let main = print_int (max 17 42)
