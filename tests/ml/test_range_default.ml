(* Expected: 0 *)
(* Test: Range pattern default case - value outside all ranges *)

let categorize x = match x with
  | 0..10 -> 1
  | 10..20 -> 2
  | _ -> 0

let main = print_int (categorize 25)
