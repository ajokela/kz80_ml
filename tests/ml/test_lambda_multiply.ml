(* Test: Lambda with multiplication *)
(* Expected: 42 *)
(* (fun x -> x * 2) 21 = 42 *)

let main = print_int ((fun x -> x * 2) 21)
