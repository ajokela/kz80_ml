(* Test: Closure that captures a multiplier *)
(* Expected: 30 *)

let make_mult m = fun x -> x * m

let triple = make_mult 3

let main = print_int (triple 10)
