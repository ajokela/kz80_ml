(* Expected: 0 *)
(* Test: Pattern guard falling through to default *)

let sign x = match x with
  | n when n > 0 -> 1
  | n when n < 0 -> -1
  | _ -> 0

let main = print_int (sign 0)
