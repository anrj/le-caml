(*Student Database*)
type student = {first_name : string; last_name : string; id : int; semester : int; grades : (int * float) list}
type database = student list

let insert : student -> database -> database = fun st db -> st :: db

let rec find_by_id : int -> database -> student list = fun id db -> 
match db with
| [] -> []
| st::sts -> if st.id = id then [st] else find_by_id id sts

let rec find_by_last_name : string -> database -> student list = fun ln db ->
match db with
| [] -> []
| st::sts -> if st.last_name = ln then st :: find_by_last_name ln sts else find_by_last_name ln sts

let remove_by_id : int -> database -> database = fun id db ->
let rec aux db acc =
match db with
| [] -> acc
| st::sts -> if st.id = id then aux sts acc else aux sts (st::acc)
in List.rev @@ aux db []

let count_in_semester : int -> database -> int = fun sem db ->
let rec aux sem db acc = 
match db with 
| [] -> acc
| st::sts -> if st.semester = sem then aux sem sts (acc + 1) else aux sem sts acc
in aux sem db 0

(*Not dividing by count / not average but total*)
let rec student_avg_grade : int -> database -> float = fun id db ->
match db with
| [] -> 0.0
| st::sts -> if st.id = id then List.fold_left (fun acc (_, g) -> acc +. g) 0.0 st.grades else student_avg_grade id sts

let course_avg_grade : int -> database -> float = fun crs db ->
let rec aux crs db acc count =
match db with
| [] -> if count = 0 then 0.0 else acc /. float_of_int count
| st::sts -> 
    let rec check_grades grades acc count =
    match grades with
    | [] -> (acc, count)
    | (c, g)::t ->
        if c = crs
        then check_grades t (acc +. g) (count + 1)
        else check_grades t acc count
in let (acc', count') = check_grades st.grades acc count
in aux crs sts acc' count'
in aux crs db 0.0 0

(*List Mishmash*)
let rec interleave3 lst1 lst2 lst3 = match lst1 with    
| [] -> begin match (lst2, lst3) with
        | ([], []) -> []
        | ([], ys) | (ys, []) -> ys
        | (x::xs, ys) -> x :: interleave3 [] ys xs
end
| x :: xs -> x :: interleave3 lst2 lst3 xs


(*Ocamlification*)
let foo x y b =
let rec bar x y b = match (x<y) with
| true -> if b then bar (x+1) y false else bar x (y-1) true
| false -> x
in match (x>y) with
| true -> bar y x b
| false -> bar x y b

(*Polynomial Party*)
let eval_poly x coefficients = 
let rec aux x c acc = match c with
| [] -> acc
| c::cs -> aux x cs (c +. x *. acc)
in
aux x (List.rev coefficients) 0.

(*let derive_poly coefficients =
let rec aux c = match c with
| [] -> ([], -1)
| c::cs -> let (result, degree) = aux c in
    if degree = -1 then ([], 0)
    else ((c *. (float_of_int (degree + 1)))::result, degree + 1)
in
let (result, _) = aux c in result
*)
let rec derive_poly2 = function
| [] -> [] 
| [h] -> []  
| h::t -> (h *. (float_of_int (List.length t))) :: derive_poly2 t
 
(*¯\_(ツ)_/¯*)
