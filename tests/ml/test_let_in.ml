(* Test: Let-in expression *)
(* Expected: 36 *)
(* quadratic(1, 2, 1, 5) = 1*25 + 2*5 + 1 = 25 + 10 + 1 = 36 *)

let quadratic a b c x =
  let x2 = x * x in
  let ax2 = a * x2 in
  let bx = b * x in
  ax2 + bx + c

let main = print_int (quadratic 1 2 1 5)
