(* Factorial function - classic recursive example *)

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let main = print_int (factorial 6)
