(* Test: Function composition *)
(* Expected: 25 *)
(* let compose f g x = f (g x) *)
(* let double x = x * 2 *)
(* let add5 x = x + 5 *)
(* compose double add5 10 = double (add5 10) = double 15 = 30 *)
(* Oops, let me recalculate: compose add5 double 10 = add5 (double 10) = add5 20 = 25 *)

let compose f g x = f (g x)
let double x = x * 2
let add5 x = x + 5
let main = print_int (compose add5 double 10)
