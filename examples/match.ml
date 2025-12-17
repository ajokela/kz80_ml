(* Pattern matching example *)

let rec collatz n =
  match n with
  | 1 -> 0
  | _ -> if n / 2 * 2 == n  (* even check *)
         then 1 + collatz (n / 2)
         else 1 + collatz (3 * n + 1)

(* Collatz conjecture: how many steps to reach 1? *)
let main = print_int (collatz 27)
