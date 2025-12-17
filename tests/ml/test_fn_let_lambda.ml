(* Test: Store lambda in variable and call it *)
(* Expected: 11 *)
(* let f = fun x -> x + 1 in f 10 = 11 *)

let f = fun x -> x + 1
let main = print_int (f 10)
