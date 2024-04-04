let is_prime n = match n with
| n when n <= 1 -> false
| n when n = 2 -> true
| n -> begin 
  let rec divide d = 
  if d * d > n then true else
    if n mod d = 0 then false else
      divide (d+1) in 
      divide 2
    end
