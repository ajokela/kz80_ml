(* Test: Function composition *)
(* Expected: 24 *)
(* double(triple(4)) = double(12) = 24 *)

let double x = x * 2
let triple x = x * 3
let main = print_int (double (triple 4))
