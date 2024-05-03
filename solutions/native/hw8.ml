let fac n = 
  let rec aux n acc =
  if n < 2 then acc else aux (n-1) (acc * n)
in aux n 1

(*let interleave3 lst1 lst2 lst3 = 
  let rec interleave2 lst1 lst2 lst3 acc = match lst1 with    
| [] -> begin match (lst2, lst3) with
        | ([], []) -> acc
        | ([], ys) | (ys, []) -> ys :: acc
        | (x::xs, ys) -> interleave2 [] ys xs (x::acc)
end
| x :: xs -> interleave2 lst2 lst3 xs (x::acc)
in List.rev @@ interleave2 lst1 lst2 lst3 []*)

