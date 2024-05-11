(*Assignment 4*) (*+2 points*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec list_of_tree_post tr = 
  match tr with
  | Lf -> []
  | Br (label, l, r) -> list_of_tree_post l @ list_of_tree_post r @ [label]

(*Assignment 3*)
type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo
let extract_min tree = match tree with
| Null -> None
| One (x, tree) -> if tree = Null then x  else x
| Two (treel, a, treer) -> a

let rec verify tree = 
  match tree with
  | Null -> true
  | One (a, tree) ->
  | Two (t1, a, t2) -> if a > 
