(* Test: Some value creation and pattern match *)
(* Expected: 42 *)
(* match Some(42) with Some(x) -> x | None -> 0 *)

let x = Some(42)
let main = print_int (match x with
  | Some(v) -> v
  | None -> 0)
