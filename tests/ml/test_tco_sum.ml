(* Expected: 5050 *)
(* Test: Tail-recursive sum with accumulator (TCO enabled) *)

let rec sum n acc = if n <= 0 then acc else sum (n - 1) (acc + n)

let main = print_int (sum 100 0)
