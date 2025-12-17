(* Test: Parentheses override precedence *)
(* Expected: 20 *)
(* (2 + 3) * 4 = 5 * 4 = 20 *)

let main = print_int ((2 + 3) * 4)
