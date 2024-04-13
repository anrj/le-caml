let x = read_int () in
let xm = ref x in
let y = ref [] in
while !xm > 0 do
  y := !xm mod 3 :: !y;
  xm := !xm / 3
done; 
!y

