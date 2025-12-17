(* Test: Simple closure that captures a variable *)
(* Expected: 15 *)

let make_adder n = fun x -> x + n

let add5 = make_adder 5

let main = print_int (add5 10)
