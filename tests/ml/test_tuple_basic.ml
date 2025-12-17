(* Expected: 3 *)
(* Basic tuple pattern matching *)

let pair = (1, 2)

let main = print_int (match pair with
  | (x, y) -> x + y)
