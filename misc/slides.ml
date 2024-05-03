(*
'a queue datastructure
*)

type 'a queue = 'a list

let enqueue e q = q @ [e]
let dequeue q = match q with
| [] -> None, []
| x::xs -> Some x, xs
let is_empty q = if q = [] then true else false



