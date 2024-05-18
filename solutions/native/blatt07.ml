(*Fun with folding*)
let f1 acc x = acc + 1

let f2 acc x = if List.length x > List.length acc then x else acc

(*let list_longest lst = 
  let rec aux acc = function
  | [] -> acc
  | h::t::r -> if List.length h >= List.length t then aux (h::acc) (h::r) else aux acc (t::r)
  | h::t -> aux (h::acc) t
in aux [] lst |> List.hd*)

let f3 acc (a, b) = acc @ [(b, a)]

let f4 acc x = x :: List.rev acc

let f5 acc (k,v) = fun x -> if x = k then v else acc k

let f6 acc f = f List.hd :: acc

let f7 acc x = acc * acc * x

(*Expression Evaluation II*)
(*I ain't reading allat*)

(*Shifting Sands*)
type graph = (int * float * int) list

(*Ar vici prim's algorithm*)