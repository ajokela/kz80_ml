(* Expected: 1 *)
(* Test: Basic range pattern matching *)

let categorize x = match x with
  | 0..10 -> 1
  | 10..20 -> 2
  | _ -> 0

let main = print_int (categorize 5)
