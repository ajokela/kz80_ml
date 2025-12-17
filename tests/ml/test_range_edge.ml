(* Expected: 2 *)
(* Test: Range pattern at boundary (end is exclusive, so 10 matches second range) *)

let categorize x = match x with
  | 0..10 -> 1
  | 10..20 -> 2
  | _ -> 0

let main = print_int (categorize 10)
