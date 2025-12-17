(* Test: Apply a function twice *)
(* Expected: 14 *)
(* let twice f x = f (f x) *)
(* let add3 x = x + 3 *)
(* twice add3 8 = add3 (add3 8) = add3 11 = 14 *)

let twice f x = f (f x)
let add3 x = x + 3
let main = print_int (twice add3 8)
