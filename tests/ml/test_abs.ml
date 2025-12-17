(* Test: Absolute value function *)
(* Expected: 7 *)
(* abs(-7) = 7 *)

let abs x = if x < 0 then 0 - x else x
let main = print_int (abs (0 - 7))
