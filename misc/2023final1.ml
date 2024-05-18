(*Assignment 4*) (*+2 points*)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec list_of_tree_post tr = 
  match tr with
  | Lf -> []
  | Br (label, l, r) -> list_of_tree_post l @ list_of_tree_post r @ [label]

(*Assignment 3*)
type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo
let rec extract_min = function
| Null -> None, Null
| One (a, tree) ->
begin match tree with
  | Null -> Some a, Null
  | One (b, tree) as r -> extract_min r
  | Two (lt, t, rt) -> extract_min lt
end
| Two (lt, t, rt) -> 
begin match lt with
  | Null -> Some t, rt
  | One (a, tree) as l -> extract_min l
  | Two (lt1, t1, lt2) as k -> extract_min k 
end

let rec verify = function
| Null -> true
| One (a, r) -> 
begin match r with
  | Null -> true
  | One (b, t) -> if b < a then verify t else false
  | Two (t1, t, t2) -> if t < a then verify t1 else false
end
| Two (t1, a, t2) -> verify t1 && verify t2 && 
begin match t1, t2 with
  | Null, Null -> true
  | One (b, _), _ | Two (_, b, _), _ -> b < a
  | _, One (c, _) | _, Two (_, c, _) -> a < c
end

let rec normal = function
  | Null -> Null
  | Two (t1, t, t2) -> Two (normal t1, t, normal t2)
  | One (a, tree) -> Two (tree, a, Null)

let rec from_list list = match List.sort_uniq compare list with