(* Test: Lambda in complex expression *)
(* Expected: 30 *)
(* (fun x -> x * x) 5 + 5 = 25 + 5 = 30 *)

let main = print_int ((fun x -> x * x) 5 + 5)
