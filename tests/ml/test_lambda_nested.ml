(* Test: Nested lambda - inner uses own parameter only *)
(* Expected: 15 *)
(* (fun x -> (fun y -> y + 5) x) 10 = (fun y -> y + 5) 10 = 15 *)
(* Note: Closures not supported - inner lambda cannot capture outer variables *)

let main = print_int ((fun x -> (fun y -> y + 5) x) 10)
