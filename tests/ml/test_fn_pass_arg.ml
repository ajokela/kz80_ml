(* Test: Pass a function as an argument *)
(* Expected: 25 *)
(* let apply f x = f x *)
(* let square x = x * x *)
(* apply square 5 = 25 *)

let apply f x = f x
let square x = x * x
let main = print_int (apply square 5)
