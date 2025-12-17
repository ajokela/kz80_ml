(* Test: Recursive factorial *)
(* Expected: 120 *)
(* 5! = 5 * 4 * 3 * 2 * 1 = 120 *)

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let main = print_int (factorial 5)
