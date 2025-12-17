(* Test: Pass a lambda directly as an argument *)
(* Expected: 36 *)
(* let apply f x = f x *)
(* apply (fun n -> n * n) 6 = 36 *)

let apply f x = f x
let main = print_int (apply (fun n -> n * n) 6)
