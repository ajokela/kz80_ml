(* Test: Option map-like function *)
(* Expected: 20 *)
(* let map_opt f opt = match opt with Some(x) -> Some(f x) | None -> None *)
(* map_opt (fun x -> x * 2) (Some 10) = Some 20 *)

let map_opt f opt = match opt with
  | Some(x) -> Some(f x)
  | None -> None

let double x = x * 2
let result = map_opt double (Some(10))
let main = print_int (match result with
  | Some(v) -> v
  | None -> 0)
