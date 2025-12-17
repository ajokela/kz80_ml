(* Test: Multiple closures with different captured values *)
(* Expected: 30 *)

let make_adder n = fun x -> x + n

let add3 = make_adder 3
let add7 = make_adder 7

let main = print_int (add3 10 + add7 10)
