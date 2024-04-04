(*Doesn't work for [1; 1; 2; 5; 3; 1]*)
let has_peak lst =
  let rec is_ascent = function
    | h :: t :: r -> if h < t then is_ascent (t :: r) else is_descent (t :: r)
    | _ -> false
  and is_descent = function
    | h :: t :: r -> if h > t then is_end_of_descent r else false
    | _ -> false
  and is_end_of_descent = function
    | _ :: [] -> true
    | _ -> false
  in
  match lst with
  | [] | [_] -> false
  | h :: t :: r -> is_ascent (h :: t :: r)

(* Test cases *)
let () =
  assert (not (has_peak [2; 1])); (* false *)
  assert (not (has_peak [3; 5; 6; 7; 5; 7; 1])); (* false *)
  assert (has_peak [3; 5; 6; 7; 5; 3; 1]); (* true *)
  print_endline "All test cases passed."
