(*Polymorphic trees*)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree

let rec insert cmp el tree =
  match tree with
  | Empty -> Node (el, Empty, Empty)
  | Node (the, left, right) -> if cmp el the = 0 then tree else 
   if cmp el the < 0 then Node (the, insert cmp el left, right) else Node (the, left, insert cmp el right)

let rec string_of_tree tostring = function
| Empty -> "Empty"
| Node (t, l, r) -> "Node (" ^ tostring t ^ ", " ^ string_of_tree tostring l ^ ", " ^ string_of_tree tostring r ^ ")"
 
let rec inorder_list = function
| Empty -> []
| Node (t, l, r) -> inorder_list l @ [t] @ inorder_list r

let inorder_list_tr tree =
  let rec aux acc stack = function
    | Empty -> begin match stack with
               | [] -> acc
               | Node (t, _, r) :: tail -> aux (t :: acc) tail r
               | _ -> failwith "Unexpected"
               end
    | Node (t, l, r) -> aux acc (Node (t, l, r) :: stack) l
in List.rev (aux [] [] tree)

(*let inorder_list_tr_pre_order_traversal tree = 
  let rec aux acc = function
    | [] -> List.rev acc
    | Empty :: rest -> aux acc rest
    | Node (t, l, r) :: rest -> aux (t :: acc) (l :: r :: rest)
in aux [] [tree]*)


(*Infinite trees*)

type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

let rec layer_tree r = LNode (r, (fun () -> layer_tree (r+1)), (fun () -> layer_tree (r+1)))

let rec interval_tree l h = LNode ((l, h), (fun () -> interval_tree l ((l+h)/2)), (fun () -> interval_tree ((l+h)/2) h))

let rec rational_tree n d = LNode ((n, d), (fun () -> rational_tree n (d+1)), (fun () -> rational_tree (n+1) d))



let find p t = let rec help = function | [] -> failwith "Unexpected" | LNode (a, l, r)::t as tree -> 
if p a then tree else help (t @ [l (); r ()])
in help [t]

let pred x = if x = 6 then true else false