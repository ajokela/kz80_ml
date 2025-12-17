(* Test: None value creation and pattern match *)
(* Expected: -1 *)
(* match None with Some(x) -> x | None -> -1 *)

let x = None
let main = print_int (match x with
  | Some(v) -> v
  | None -> -1)
