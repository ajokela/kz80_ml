(* Invoice calculation with decimal arithmetic *)
(* Demonstrates auto-coercion from int to decimal *)

let price = 19.99
let quantity = 5

(* quantity (int) is auto-coerced to decimal *)
let subtotal = price * quantity

let tax_rate = 0.0825
let tax = subtotal * tax_rate

let total = subtotal + tax

let main = print_decimal total
