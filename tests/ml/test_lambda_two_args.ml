(* Test: Lambda with two arguments *)
(* Expected: 17 *)
(* (fun a b -> a + b) 5 12 = 17 *)

let main = print_int ((fun a b -> a + b) 5 12)
