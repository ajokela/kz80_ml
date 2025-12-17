(* Test: Nested if-then-else - negative case *)
(* Expected: -1 *)
(* sign(-5) = -1 *)

let sign x = if x < 0 then 0 - 1 else if x == 0 then 0 else 1

let main = print_int (sign (0 - 5))
