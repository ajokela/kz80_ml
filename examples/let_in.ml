(* Local let bindings *)

let quadratic a b c x =
  let x2 = x * x in
  let ax2 = a * x2 in
  let bx = b * x in
  ax2 + bx + c

(* Evaluate x^2 + 2x + 1 at x = 5 *)
(* Expected: 25 + 10 + 1 = 36 *)
let main = print_int (quadratic 1 2 1 5)
