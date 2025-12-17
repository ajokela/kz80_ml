(* Expected: 5040 *)
(* Test: Tail-recursive factorial with accumulator (TCO enabled) *)
(* 7! = 5040 *)

let rec factorial n acc = match n with
  | 0 -> acc
  | 1 -> acc
  | _ -> factorial (n - 1) (acc * n)

let main = print_int (factorial 7 1)
