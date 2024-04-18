(*Peano Arithmetic*)
type natural = Zero | Successor of natural

let rec natural_of_int integer = match integer with 
| 0 -> Zero
| n when n > 0 -> Successor (natural_of_int (n-1))
| _ -> failwith "Has to be a number >= 0"

let rec int_of_natural natural =
  let rec aux natural acc = match natural with
| Zero -> 0
| Successor tail -> if tail = Zero then acc + 1 else aux tail (acc + 1)
in
aux natural 0;;

let rec add_natural nat1 nat2 = match (nat1, nat2) with
| (Zero, n) | (n, Zero) -> n
| (Successor tail, Successor Zero) | (Successor Zero, Successor tail) -> Successor (Successor tail)
| (Successor tail1, n) -> Successor (add_natural tail1 n)

let rec multiply_natural nat1 nat2 = match (nat1, nat2) with
| (Zero, n) | (n, Zero) -> Zero
| (Successor Zero, n) | (n, Successor Zero) -> n
| (Successor tail, Successor tail2) -> add_natural (Successor tail2) (multiply_natural (tail) (Successor tail2))

let rec power_natural nata natb = match natb with
| Zero -> Successor Zero
| Successor tail -> multiply_natural nata (power_natural nata tail)

let rec leq_natural nata natb = match nata, natb with
| Zero, Zero -> true
| Zero, Successor Zero -> true
| Successor tail1, Successor tail2 -> leq_natural tail1 tail2
| _ -> false


type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)(**
let print_tree filename btree = 
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty -> 
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in 
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file
*)

(*Crawling Trees*)

let crawl : command list -> tree -> tree = fun cmd current ->
  let rec aux cmd current stack = match cmd with
| [] -> current
| Left  ::tail -> aux tail (match current with Node (_, l, _) -> l | Empty -> failwith "Invalid") (current::stack)
| Right ::tail -> aux tail (match current with Node (_, _, r) -> r | Empty -> failwith "Invalid") (current::stack)
| Up    ::tail -> begin match stack with [] -> failwith "Invalid" | h::t -> aux tail h t end
| New x ::tail -> aux tail (Node (x, Empty, Empty)) (current::stack) (*todo*)
| Delete::tail -> aux tail Empty (current::stack) (*todo*)
| Push  ::tail -> aux tail current (current::stack)
| Pop   ::tail -> begin match stack with [] -> failwith "Invalid" | h::t -> h end
in aux cmd current []

(*(Node (1, Empty, Node (2, Node (3, Empty, Empty), Node (4, Empty, Node(5, Node(6, Node(7, Empty, Empty), Empty), Empty)))))*)



(*Quadtrees*)
type quadtree_node = NoPoint
                   | Point of int * int
                   | QNode of quadtree_node (* bottom left *)
                            * quadtree_node (* top left *)
                            * quadtree_node (* bottom right *)
                            * quadtree_node (* top right *)

type quadtree = {width:int; height:int; root:quadtree_node}
type point = int * int

let insert : point -> quadtree -> quadtree = fun point tree ->
  let rec aux point dimensions node = match point, dimensions, node with
  | (x, y), (x0, y0, width, height), node ->
  let xmid = (width + x0) / 2 in 
  let ymid = (height + y0) / 2 in
  if (x >= width || x <= x0) && (y >= height || y <= y0) then failwith "Point out of bounds" 
  else match node with
  | NoPoint -> Point (x, y)
  | QNode (l1, l2, r1, r2) -> 
  begin match x < width/2, y < height/2 with
  | true, true   -> QNode ((aux (x,y) (x0, y0, xmid, ymid) l1), l2, r1, r2)
  | true, false  -> QNode (l1, (aux (x,y) (x0, ymid, xmid, height) l2), r1, r2)
  | false, true  -> QNode (l1, l2, (aux (x,y) (xmid, y0, width, ymid) l1), r2)
  | false, false -> QNode (l1, l2, r1, (aux (x,y) (xmid, ymid, width, height) l1))
  end
  | Point (m, n) -> if (x, y) = (m, n) then Point (x, y) 
  else aux (x, y) (x0, y0, width, height) (aux (m, n) (x0, y0, width, height) (QNode (NoPoint, NoPoint, NoPoint, NoPoint))) 
 in let groot = aux point (0, 0, tree.width, tree.height) tree.root
in {width = tree.width; height = tree.height; root = groot}