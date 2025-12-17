(* Expected: 42 *)
(* Tuple with wildcard pattern *)

let pair = (100, 42)

let main = print_int (match pair with
  | (_, y) -> y)
