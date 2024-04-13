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



(*TTF exercises*)


