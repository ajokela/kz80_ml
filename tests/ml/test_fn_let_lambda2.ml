(* Test: Lambda with two args stored in variable *)
(* Expected: 17 *)
(* let add = fun a b -> a + b in add 5 12 = 17 *)

let add = fun a b -> a + b
let main = print_int (add 5 12)
