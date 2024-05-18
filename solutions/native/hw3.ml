(****************************************************** 

  ASSIGNMENT 1. DUE APRIL 23, 2023 

  Write your solutions directly in this file, under each problem.

  Total points: 5

  ****************************************************************************) 


(*    1.  (0.3 points) 

   Write a function member, which takes a comparision function c, a term t and 
   a list l and returns true if l contains an element e such that e and t are
   equal with respect to c.

   To remind for the sample runs below: the built-in compare function gives 
       compare t e = 0
   if t and e are equal. 

   # member compare 3 [1; 2; 3];;
   - : bool = true
   # member compare 4 [1; 2; 3];;
   - : bool = false
   # member compare 'a' ['a'; 'b'; 'c'];;
   - : bool = true

   If a comparison function equal_second_components is defined as
      let equal_second_components (_, x) (_, y) = compare x y 
   then your function should behave, e.g., as follows:

   # member equal_second_components ('a',5) [(1,2); (3,4); (5,6)];;
   - : bool = false
   # member equal_second_components ('a',6) [(1,2); (3,4); (5,6)];;
   - : bool = true
   # member equal_second_components (42, 6) [(1,2); (3,4); (5,6)];;
   - : bool = true

   If a comparision function evens_eq_evens_odds_eq_odds makes all even
   numbers equal to each other and all odd numbers equal to each other, 
   e.g., by defining
      let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)
   then your function should behave, e.g., as follows:
 
   # member evens_eq_evens_odds_eq_odds 4 [1; 2; 3];;
   - : bool = true
   # member evens_eq_evens_odds_eq_odds 4 [1; 3; 5];;
   - : bool = false

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)
let equal_second_components (_, x) (_, y) = compare x y 
let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)
let rec member c term lst = match lst with | [] -> false | h::t -> if c term h = 0 then true else member c term t


let testing_member () =
   let l =
     [
       __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
       __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
       __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
       __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
       __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
       __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
       __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
       __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
    ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The member test succeeds.\n"; [])
   else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* ********************************************************************************** *)

(*    2.  ( 0.5 points)

   Write a function count_occurrences, which takes a list list1 and returns 
   a list list2 of pairs (e,n), where e is an element of list1 and n is the 
   number of occurrences of e in list1. Make sure that list2 is given in a 
   sorted form, in the descending order of the occurrences.

   Sample runs:

   # count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd'];;
   - : (char * int) list = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]

   # count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0];;
   - : (int * int) list = [(0, 4); (3, 3); (-1, 2); (-2, 1)]

   # count_occurrences 
         [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)];;
   - : ((string * int) * int) list =
         [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)

let rec helper_count element list3 = match list3 with
| [] -> [(element, 1)]
| (h,n)::tail -> if element = h then (element, n+1)::tail else (h, n)::helper_count element tail 

let count_occurrences list1 =
  let rec aux acc list1 = match list1 with
  | [] -> acc
  | h :: t -> aux (helper_count h acc) t
in List.sort (fun (_, a) (_, b) -> compare b a) (aux [] list1)


let testing_count_occurrences () =
   let l =
     [
       __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
       __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
       __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
       __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
   else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* ********************************************************************************** *)

(*     3.  (0.5 points)
 * 
 *  Write a function drop_last : 'a list -> 'a list which takes a list,
 *  drops its last element and gives beck the remaining part.
 *
 *  Sample runs:
      # drop_last [1; 2; 3; 4];;
      - : int list = [1; 2; 3]
      # drop_last [1];;
      - : int list = []
      # drop_last [];;
      Exception: Failure "Empty list has no last element".

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)                                      
 
 let drop_last lst =
  let rec aux lst acc = match lst with
  | [] -> failwith "Empty list has no last element"
  | [x] -> acc
  | h::t -> aux t (h :: acc)
  in List.rev @@ aux lst []


let testing_drop_last () =
   let l =
     [
       __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
       __LINE_OF__ ((drop_last [1]) = []);
       __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The drop_last test succeeds.\n"; [])
   else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* ********************************************************************************** *)
                                     
(*     4. (0.2 point)
 * 
 *  Modify drop_last so that it returns an optional value, instead of raising
 *  an exception for empty lists. Let this variant be called drop_last_opt.
 *  Then its type should be 'a list -> 'a list option and sample runs look as
      #  drop_last_opt [];;
      - : 'a list option = None
      #  drop_last_opt [1];;
      - : int list option = Some []
      #  drop_last_opt [1;2;3];;
      - : int list option = Some [1; 2]

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)

let drop_last_opt lst = if lst = [] then None else Some (drop_last lst)
let testing_drop_last_opt () =
   let l =
     [
       __LINE_OF__ ((drop_last_opt []) = None);
       __LINE_OF__ ((drop_last_opt [1]) = Some []);
       __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
   else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* ********************************************************************************** *)
                  
(*     5. (0.5 points)
 * 
 *  Write a function zip_with that, given a binary function and two lists,
 *  constructs a new list as in:
 *
 *    zip_with f [x1; ...; xm] [y1; ...; yn] = [f x1 y1; ...; f xk yk]
 *
 *  where k is the minimum between n and m.
 *  
 *  Sample runs:
      # zip_with (fun x y -> [x;y]) [1;2;3] [5;6];;
      - : int list list = [[1; 5]; [2; 6]]
      # zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8];;
      - : int list list = [[1; 5]; [2; 6]; [3; 7]]
      # zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b'];;
      - : (int * int) list = [(1, 'a'); (2, 'b')]
      # zip_with (+) [1;2;3] [5;6];;
      - : int list = [6; 8]
      # zip_with (^) ["aa";"bb";"cc"] ["1";"2"];;
      - : string list = ["aa1"; "bb2"]

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)


let zip_with f lst1 lst2 = 
  let rec aux acc lst1 lst2 = match lst1, lst2 with
| [], [] -> acc
| (x, []) -> acc
| ([], x) -> acc
| x::xs, y::ys -> aux ((f x y) :: acc) xs ys
in List.rev (aux [] lst1 lst2)


let testing_zip_with () =
   let l =
     [
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
       __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
       __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
       __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);
       
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The zip_with test succeeds.\n"; [])
   else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
          
                  
(* ********************************************************************************** *)
       
(*     6. (0.5 points)
 * 
 *  Write a function unzip on lists (i.e., a list of pairs is ‘unzipped’ into
 *  a pair of lists) using one of the fold functions you already know.
 *  For instance, unzip [('a',1); ('b',2)] = (['a';'b'], [1;2])

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)

let rec unzip lst1 = match lst1 with
| [] -> ([], [])
| (x::x')::tail -> let (y, y') = unzip lst1 in (x::y, x'::y')
| _ -> failwith "Unexpected"

(*let testing_unzip () =
   let l =
     [
       __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
       __LINE_OF__ ((unzip []) = ([], []));
       __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));
       
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The unzip test succeeds.\n"; [])
   else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
*)

(* ********************************************************************************** *)

(*     7. (0.2 points)
 *
 *  Show the evaluation steps of unzip [('a',1);('b',2)]
 *)  



(* Problem 8: (2.5 points).  (1.25 points for the table, 1.25 for the scorers)
 * -----------------------------------------------------------------------------
 *
 * Assume an information about a football game is given by a tuple
 *    (team1, scorers1, team2, scorers2)
 * where scorers1 (resp. scorers2) is the list of players who scored goals in 
 * this game for team1 (resp. for team2).
 *
 * For instance, the information about the World Cup 2018 final game, 
 * France - Croatia 4-2, would be given like this:
 * 
 * (Fra, ["OG"; "Griezmann"; "Pogba"; "Mbappe"], Cro, ["Perisic"; "Mandzukic"])
 *
 * "OG" indicates that it was an own-goal by a Croatian player, without giving 
 * the name.
 *
 * We assume that a list of such game data is given. They give information 
 * about games in a round-robin tournament (e.g., World Cup or Euro groups,
 * or league games).
 *
 * Your task is to write a function table_and_scorers, which takes the list of
 * above defined tuples and returns two lists:
 *
 * (a) The list of tuples for each team of the form
 *            (t, g, w, d, l, gf, ga, p),
 *   where t stands for a team, g for games played, w for wins, d for draws,
 *   l for losses, gf for goals for (scored goals), ga for goals against
 *   (conseded goals), p for points, which records the summary of all the games 
 *   played by the team.
 *
 *   Sort the list of such tuples
 *     - first by points, 
 *     - in case of a tie by goal difference (gf - ga), 
 *     - in case of a tie by goals_for,
 *     - in case of a tie randomly. 
 *
 *   In other words, the first task in to construct the tournament table.
 *
 * (b) The list of goalscorers as triples (player, team, goals). Sort the list 
 *     first by goals, in case of a tie by the player name alphabetically. 
 * 
 * Example:
 *   Assume that the type team is defined as
 
     type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

 *  and the information about WC 2023 C and H group games 
 *  (https://en.wikipedia.org/wiki/2022_FIFA_World_Cup#Group_C
 *   https://en.wikipedia.org/wiki/2022_FIFA_World_Cup#Group_H) 
 *
 *  is given as

 let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]

 let wc22_H = 
  [(Uru, [], Kor, []);
   (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
   (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
   (Por, ["Fernandes"; "Fernandes"], Uru, []);
   (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
   (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ]

 * Your function table_and_scorers should behave, e.g., as follows:

   # table_and_scorers wc22_C;;
   - : (team * int * int * int * int * int * int * int) list *
       (string * team * int) list
   =
   ([(Arg, 3, 2, 0, 1, 5, 2, 6); (Pol, 3, 1, 1, 1, 2, 2, 4);
     (Mex, 3, 1, 1, 1, 2, 3, 4); (Sau, 3, 1, 0, 2, 3, 5, 3)],
    [("Al-Dawsari", Sau, 2); ("Messi", Arg, 2); ("Al-Shehri", Sau, 1);
     ("Alvarez", Arg, 1); ("Chavez", Mex, 1); ("Fernandez", Arg, 1);
     ("Lewandowski", Pol, 1); ("Mac Allister", Arg, 1); ("Martin", Mex, 1);
     ("Zielinski", Pol, 1)])

   # table_and_scorers wc22_H;;
   - : (team * int * int * int * int * int * int * int) list *
       (string * team * int) list
   =
   ([(Por, 3, 2, 0, 1, 6, 4, 6); (Kor, 3, 1, 1, 1, 4, 4, 4);
     (Uru, 3, 1, 1, 1, 2, 2, 4); (Gha, 3, 1, 0, 2, 5, 7, 3)],
    [("Cho Gue-sung", Kor, 2); ("De Arrascaeta", Uru, 2); ("Fernandes", Por, 2);
     ("Kudus", Gha, 2); ("Ayew", Gha, 1); ("Bukari", Gha, 1); ("Felix", Por, 1);
     ("Horta", Por, 1); ("Hwang Hee-chan", Kor, 1); ("Kim Young-gwon", Kor, 1);
     ("Leao", Por, 1); ("Ronaldo", Por, 1); ("Salisu", Gha, 1)])

 *
 * WRITE YOUR IMPLEMENTATION BELOW. CHECK IT WITH THE PROVIDED TESTS. 
 *)

(* ------------------------------------------------------------------------
 *  Defining football team type
 * ------------------------------------------------------------------------
 *)

type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

                                                        
(* ------------------------------------------------------------------------
 *  Sample data, from the groups of the WC 2022 tournament
 * ------------------------------------------------------------------------
 *)

let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]

let wc22_H = 
  [(Uru, [], Kor, []);
   (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
   (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
   (Por, ["Fernandes"; "Fernandes"], Uru, []);
   (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
   (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ]

let scorers team scorers =
  let sorted_list = count_occurrences scorers in 
  let rec aux = function
  | [] -> []
  | (player, goals)::t -> (player, team, goals)::aux t 
in List.sort (fun (n1, _, a) (n2, _, b) -> if a = b then compare n1 n2 else compare b a) (aux sorted_list)


(*let table_and_scorers = function
| [] -> 
| (team1, scorers1, team2, scorers2)::tail -> ( ,((scorers team1 scorers1) @ (scorers team2 scorers2))) *) 


let testing_table_and_scorers () =
  
    [
      __LINE_OF__ (table_and_scorers wc22_H =
                     ([(Por, 3, 2, 0, 1, 6, 4, 6);
                       (Kor, 3, 1, 1, 1, 4, 4, 4);
                       (Uru, 3, 1, 1, 1, 2, 2, 4);
                       (Gha, 3, 1, 0, 2, 5, 7, 3)],
                      [("Cho Gue-sung", Kor, 2);
                       ("De Arrascaeta", Uru, 2);
                       ("Fernandes", Por, 2);
                       ("Kudus", Gha, 2);
                       ("Ayew", Gha, 1);
                       ("Bukari", Gha, 1);
                       ("Felix", Por, 1);
                       ("Horta", Por, 1);
                       ("Hwang Hee-chan", Kor, 1);
                       ("Kim Young-gwon", Kor, 1);
                       ("Leao", Por, 1);
                       ("Ronaldo", Por, 1);
                       ("Salisu", Gha, 1)]));
      __LINE_OF__ (table_and_scorers wc22_C =
                     ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                       (Pol, 3, 1, 1, 1, 2, 2, 4);
                       (Mex, 3, 1, 1, 1, 2, 3, 4);
                       (Sau, 3, 1, 0, 2, 3, 5, 3)],
                      [("Al-Dawsari", Sau, 2);
                       ("Messi", Arg, 2);
                       ("Al-Shehri", Sau, 1);
                       ("Alvarez", Arg, 1);
                       ("Chavez", Mex, 1);
                       ("Fernandez", Arg, 1);
                       ("Lewandowski", Pol, 1);
                       ("Mac Allister", Arg, 1);
                       ("Martin", Mex, 1);
                       ("Zielinski", Pol, 1)]))
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
  else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 