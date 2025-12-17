(* Expected: 60 *)
(* Three-element tuple *)

let triple = (10, 20, 30)

let main = print_int (match triple with
  | (x, y, z) -> x + y + z)
