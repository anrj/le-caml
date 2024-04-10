(*Reversing a list*)
let rev lst =
  let rec aux ?(acc=[]) = function
  | [] -> acc
  | h::t -> aux ~acc:(h::acc) t
in aux lst;;

(*Head of a list*)
let rec head = function
| [] -> None
| h::t -> Some h

(*Append two lists*)
let rec append lst1 lst2 = match lst1 with
| [] -> lst2
| h::t ->  h :: (append t lst2)

(*Tail of a list*)
let rec tail = function
| [] -> None
| t::[] -> Some t
| h::t -> tail t

(*Remove the K'th Element From a List*)
let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t 
  else h :: remove_at (n - 1) t

(*Return n'th element from a list*)
let rec nth n lst = match (n, lst) with
  | (_, []) -> raise (Failure "The list is empty")
  | (0, h::_) -> Some h
  | (n, _::t) -> nth (n-1) t

(*Palindrome*)
let palindrome = function
| [] -> true
| h::t -> h = List.hd (List.rev t)

(*Last two elements of a list*)
let rec tail2 = function
| [] | [_] -> None
| h::t::[] -> Some (h,t)
| h::t -> tail2 t

(*Length of a list*)
let len lst = 
  let rec aux count = function
  | [] -> count
  | h::t -> aux (count+1) t
in aux 0 lst;;

(*Insert element at K'th index in a list*)
let rec insert ~element ~index ~list = 
if index >= len list || index < 0 then failwith "Index out of bounds"
else match (index, list) with
| (0, []) -> [element]
| (0, h::t) -> element::h::t
| (index, h::t) -> h :: insert ~index:(index - 1) ~element:element ~list:t
| _ -> failwith ""

(**)

