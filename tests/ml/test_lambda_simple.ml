(* Test: Simple lambda application *)
(* Expected: 6 *)
(* (fun x -> x + 1) 5 = 6 *)

let main = print_int ((fun x -> x + 1) 5)
