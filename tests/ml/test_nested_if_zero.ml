(* Test: Nested if-then-else - zero case *)
(* Expected: 0 *)
(* sign(0) = 0 *)

let sign x = if x < 0 then 0 - 1 else if x == 0 then 0 else 1

let main = print_int (sign 0)
