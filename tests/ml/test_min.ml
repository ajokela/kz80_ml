(* Test: Min function *)
(* Expected: 17 *)

let min a b = if a < b then a else b
let main = print_int (min 17 42)
