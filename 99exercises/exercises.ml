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
let rec insert2 element n lst = 
  if n >= List.length lst || n < 0 then failwith "Index out of bounds"
  else match lst with
  | [] -> [element]
  | h::t as l -> if n = 0 then element::l else insert2 element (n-1) t
  
(*Flatten a List*)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten flist = 
  let rec aux flist acc = match flist with
  | [] -> acc
  | h::t -> begin match h with
            | One a -> aux t (acc @ [a])
            | Many lst -> aux (lst @ t) acc
            end
in aux flist []

(*Eliminate Duplicates*)
let compress =
  let rec aux acc = function
  | h::t::r -> if h = t then aux acc (h::r) else aux (acc @ [h]) (t::r)
  | lst -> acc @ lst
in aux []

(*Pack Consecutive Duplicates*)
let pack lst =
  let rec aux acc1 acc2 = function 
  | [] -> acc2
  | h::(t::r as l) -> if h = t then aux (acc1 @ [h]) (acc2) l else aux [] ((acc1 @ [h])::acc2) l
  | h::t -> if h = (List.hd acc1) then aux acc1 ((acc1 @ [h])::acc2) t else aux acc1 ([h] :: acc2) t
in (aux [] [] lst) |> List.rev

(*Run-Length Encoding*) (***)

(*let rec helper_count elm lst = match lst with
| [] -> [(elm, 1)]
| (h, n)::t -> if elm = h then (elm, n+1)::t else (h, n)::helper_count elm t

let count_occurrences lst =
  let rec aux acc lst = match lst with
  | [] -> acc
  | h::t -> aux (helper_count h acc) t
in aux [] lst*)

let encode list =
  let rec aux count acc = function
    | [] -> [] 
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((count + 1, a) :: acc) t in
  List.rev (aux 0 [] list);;

(*Modified Run-Length Encoding*)
type 'a rle =
  | One of 'a
  | Many of int * 'a
(*TODO*)

(*Decode a Run-Length Encoded List*)
let decode rle =
  let rec aux acc = function
  | [] -> acc
  | One x :: tail -> aux (x :: acc) tail
  | Many (n, x) :: tail -> aux ((List.init n (fun _ -> x)) @ acc) tail
in aux [] rle |> List.rev

(*Run-Length Encoding of a List (Direct Solution)*)
(*TODO*)

(*Duplicate the Elements of a List*)
let duplicate lst =
  let rec aux acc = function
  | [] -> acc
  | h::t -> aux (acc @ [h; h]) t
in aux [] lst

(*Replicate the Elements of a List a Given Number of Times*)
let replicate lst n =
  let rec aux acc = function
  | [] -> acc
  | h::t -> aux (acc @ (List.init n (fun _ -> h))) t
in aux [] lst

(*Drop Every N'th Element From a List*)
let drop_nth lst n = 
  let rec aux acc count = function
  | [] -> acc
  | h::t -> if count = n then aux acc 1 t else aux (h::acc) (count + 1) t
in aux [] 1 lst |> List.rev

(*Split a List Into Two Parts; The Length of the First Part Is Given*)
let split lst n =
  let rec aux acc1 acc2 count = function
  | [] -> (List.rev acc1, List.rev acc2)
  | h::t -> if count = n then aux acc1 (t::acc2) 0 [] else aux (h::acc1) acc2 (count + 1) t
in aux [] [] 0 lst 

(*Extract a Slice From a List*)
let slice lst i k = 
  let rec aux acc count = function 
  | [] -> acc
  | h::t -> if count >= i && count <= k then aux (h::acc) (count+1) t else aux acc (count+1) t
in aux [] 0 lst |> List.rev

(*Rotate a List N Places to the Left*)
let rotate lst n = 
  let rec aux acc1 acc2 count = function
  | [] -> acc2 @ List.rev acc1
  | h::t -> if count < n then aux (h::acc1) acc2 (count+1) t else aux acc1 (h::t) count []
in aux [] [] 0 lst

(*Create a List Containing All Integers Within a Given Range*)
let range i k = 
  let rec aux acc i k = if i <> k then if i < k then aux (i::acc) (i+1) k else aux (k::acc) i (k-1) else acc
in aux [] i k |> List.rev

(*Extract a Given Number of Randomly Selected Elements From a List*)
let rand_select lst n =
  let rec get n = function
  | [] -> failwith ""
  | h :: t -> if n = 0 then h else get (n-1) t in
  let rec aux acc n = function
  | [] -> acc
  | h::t -> if n = 0 then aux acc n [] else aux ((get ( Random.int @@ List.length lst) lst) :: acc) (n-1) (h::t)
in aux [] n lst

(*Lotto: Draw N Different Random Numbers From the Set 1..M*)
let lotto_select n ub =
 let rec aux acc n = if n > 0 then aux ((1 + Random.int ub) :: acc) (n - 1) else acc
in aux [] n

