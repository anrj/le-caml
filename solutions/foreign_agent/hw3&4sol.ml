(*    1. 

   Write a function member, which takes a comparision function c, a term t and 
   a list l and returns true if l contains and element e such that e and t are
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

(* -----------------------------------------------------------------------------
 *  member
 * -----------------------------------------------------------------------------
 * Testing generic membership in a list.
 *)
let rec member c term list =
  match list with
  | [] -> false
  | head :: tail -> (c head term = 0) || member c term tail

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)
let equal_second_components (_, x) (_, y) = compare x y


(*    2. 

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

(* -----------------------------------------------------------------------------
 *  count_occurrences
 * -----------------------------------------------------------------------------
 * Takes a list and returns a list of pairs consisting of an element of the
 * original list and the number of its occurrences there.
 *)
let rec increase_value key dictionary =
  match dictionary with
  | [] -> [(key, 1)]
  | (key', value') :: tail ->
     if key = key'
     then (key, value' + 1) :: tail
     else (key', value') :: increase_value key tail

                        
let count_occurrences list =
  let freq_order (_, n1) (_, n2) = compare n2 n1 in
  let rec count_occurrences_aux acc = function
    | [] -> acc
    | head :: tail -> count_occurrences_aux (increase_value head acc) tail in
  List.sort freq_order (count_occurrences_aux [] list)


(*     3. 
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

(* -----------------------------------------------------------------------------
 *  drop_last
 * -----------------------------------------------------------------------------
 * Takes a list and returns its part without the last element.
 *)
let rec drop_last = function
  | [] -> failwith "Empty list has no last element"
  | [_] -> []
  | x :: y :: tail -> x :: drop_last (y :: tail)
  
                                     
(*     4. 
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

(* -----------------------------------------------------------------------------
 *  drop_last_opt
 * -----------------------------------------------------------------------------
 * Takes a list and returns its part without the last element. Gives optional
 * value: None for the empty list and Some list for non-empty lists.
 *)
let drop_last_opt list =
  if list = [] then None else Some (drop_last list) 



                  
(*     5. 
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
                  
(* -----------------------------------------------------------------------------
 *  zip_with
 * -----------------------------------------------------------------------------
 * Zips two lists together using a binary function, stopping at the end of the 
 * shorter list.
 *
 *)
let rec zip_with f l1 l2 =
  match l1, l2 with
  | x :: xs, y :: ys -> (f x y) :: zip_with f xs ys
  | _ -> []

       
(*     6.
 * 
 *  Write a function unzip on lists (i.e., a list of pairs is ‘unzipped’ into
 *  a pair of lists) using one of the fold functions you already know.
 *  For instance, unzip [('a',1); ('b',2)] = (['a';'b'], [1;2])

 *  WRITE YOUR IMPLEMENTATION BELOW
 *)

(* -----------------------------------------------------------------------------
 *  unzip
 * -----------------------------------------------------------------------------
 * Unzips a list of pairs into two lists, one consisting of the first elements
 * and the other one consisting of the second elements of the pairs.
 *)

let rec foldr f b = function
  | [] -> b
  | x :: xs -> f x (foldr f b xs)

let unzip list =
  foldr (fun (x,y) (xs,ys) -> (x::xs,y::ys)) ([],[]) list 

  
(*     7.
 *
 *  Show the evaluation steps of unzip [('a',1);('b',2)]
 *)  

(* 
   Evaluation steps of unzip [('a',1);('b',2)]

   unzip [('a',1);('b',2)]
   foldr (fun (x,y) (xs,ys) -> (x::xs,y::ys)) ([],[]) [('a',1);('b',2)]
     Notation: f = (fun (x,y) (xs,ys) -> (x::xs,y::ys))
   f ('a',1) (foldr f ([],[]) [('b',2)]) 
   f ('a',1) (f [('b',2)] (foldr f ([],[]) [])) 
   f ('a',1) (f [('b',2)]  ([],[])) 
   f ('a',1) ('b'::[], 2::[]) 
   ('a'::'b'::[], 1::2::[]) which is the same as
   (['a';'b'], [1;2])

 *)


(* Problem 8: 
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
 *  Sample data, from the groups of the Euro 2020 tournament
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


(* ------------------------------------------------------------------------
 *  data_to_score :  'a * 'b list * 'c * 'd list ->
 *     'a * int * ('b * int) list * 'c * int * ('d * int) list
 * ------------------------------------------------------------------------
 *  Transforming game data into the score of the game 
 *  together with scorers list

 *)

let data_to_score (team1, scorers1, team2, scorers2) =
  let scorers_goals1 = List.map (fun x -> (x,1)) scorers1 in
  let scorers_goals2 = List.map (fun x -> (x,1)) scorers2 in
  (team1, List.length scorers1, scorers_goals1,
   team2, List.length scorers2, scorers_goals2)

    
(* ------------------------------------------------------------------------
 *  aggregate_goals_by_single_scorer : 
 *       'a * int -> ('a * int) list -> ('a * int) list
 * ------------------------------------------------------------------------ 
 *  In the scorers list, update information about the scorer 
 *  by adding n to the goals scored by him/her
 *)    

let rec aggregate_goals_by_single_scorer (scorer, n) scorers_list = 
  match scorers_list with 
  | [] -> [(scorer, n)]
  | ((scorer', m) as h) :: t ->
     if scorer = scorer' then (scorer, n+m) :: t
     else h :: aggregate_goals_by_single_scorer (scorer, n) t
       
  
(* ------------------------------------------------------------------------
 *  aggregate_scorers : 
 *        ('a * int) list -> ('a * int) list -> ('a * int) list
 * ------------------------------------------------------------------------ 
 *  In the scorers list, update information about scorers 
 *  by updating information about each of them 
 *)              

let rec aggregate_scorers scorers scorers_list  =
  match scorers with
  | [] -> scorers_list
  | h :: t ->
     let updated_scorers_list = aggregate_goals_by_single_scorer h scorers_list
     in aggregate_scorers t updated_scorers_list


(* ------------------------------------------------------------------------
 *  Defining team records as tuples
 * ------------------------------------------------------------------------
 *)

type scorer = string * int
                         
type team_record =
    team *        (* t:  team name         *)
    int *         (* g:  games played      *)
    int *         (* w:  games won         *)
    int *         (* d:  games drawn       *)
    int *         (* l:  games lost        *)
    int *         (* gf: goals for         *)
    int *         (* ga: goals against     *)
    int *         (* p:  points collected  *)
    scorer list   (* sc: list of scorers   *)
 

(* ------------------------------------------------------------------------
 *  Selector functions
 * ------------------------------------------------------------------------ 
 *  Selecting components from the give team record tuple
 *)  
  
let team          (t,_,_,_,_,_,_,_,_) = t
let played        (_,g,_,_,_,_,_,_,_) = g
let won           (_,_,w,_,_,_,_,_,_) = w
let drawn         (_,_,_,d,_,_,_,_,_) = d
let lost          (_,_,_,_,l,_,_,_,_) = l
let goals_for     (_,_,_,_,_,f,_,_,_) = f
let goals_against (_,_,_,_,_,_,a,_,_) = a
let points        (_,_,_,_,_,_,_,p,_) = p
let scorers_list  (_,_,_,_,_,_,_,_,s) = s

                               
(* ------------------------------------------------------------------------
 *  score_to_record_pair : 
 *  team * int * (string * int) list * team * int * (string * int) list ->
 *  team_record * team_record
 * ------------------------------------------------------------------------ 
 *  Transforming score into a pair of records for each involved team.
 *  The records contain information only about the given match
 *)  

let score_to_record_pair (team1, goals1, scorers1, team2, goals2, scorers2) =
  let scorers1_aggregated = aggregate_scorers scorers1 [] in
  let scorers2_aggregated = aggregate_scorers scorers2 [] in
  match compare goals1 goals2 with
  | 0 -> ((team1, 1, 0, 1, 0, goals1, goals2, 1, scorers1_aggregated),
          (team2, 1, 0, 1, 0, goals2, goals1, 1, scorers2_aggregated))
  | n ->
     if n < 0 then
       ((team1, 1, 0, 0, 1, goals1, goals2, 0, scorers1_aggregated),
        (team2, 1, 1, 0, 0, goals2, goals1, 3, scorers2_aggregated))
     else
       ((team1, 1, 1, 0, 0, goals1, goals2, 3, scorers1_aggregated),
        (team2, 1, 0, 0, 1, goals2, goals1, 0, scorers2_aggregated))


(* ------------------------------------------------------------------------
 *  update_record_list : 
 *      team_record -> team_record list -> team_record list
 * ------------------------------------------------------------------------ 
 *  Updating the record list by a new record.
 *)    

let rec update_record_list r = function
  | [] -> [r]
  | h :: tail ->
     if compare (team h) (team r) = 0
     then
       let scorers_aggr =
         aggregate_scorers (scorers_list h) (scorers_list r)
       in
       ((team r), (played r)+(played h), 
        (won r)+(won h), (drawn r)+(drawn h), (lost r)+(lost h),
        (goals_for r)+(goals_for h), (goals_against r)+(goals_against h),
        (points r)+(points h), scorers_aggr)
       :: tail
     else h :: update_record_list r tail
       

(* ------------------------------------------------------------------------
 *  data_to_record_list : 
 *    (team * string list * team * string list) list -> team_record list
 * ------------------------------------------------------------------------ 
 *  Transforming tournament data into the list of records for each team
 *)
                                  
let rec data_to_record_list acc data =
  match data with
  | [] -> acc
  | h :: t ->
     let score = data_to_score h in
     let (record1, record2) = score_to_record_pair score in
     let new_acc =
       update_record_list record2 (update_record_list record1 acc) in
     data_to_record_list new_acc t

                         
(* ------------------------------------------------------------------------
 *  compare_scorers : 'a * 'b * 'c -> 'a * 'd * 'c -> int
 * ------------------------------------------------------------------------ 
 *  Comparison of scorers
 *)                         

let compare_scorers (name1, _, goals1) (name2, _, goals2) =
  match compare goals2 goals1 with
  | 0 -> compare name1 name2
  | n -> n

           
(* ------------------------------------------------------------------------
 *  compare_teams : team_record -> team_record -> int
 * ------------------------------------------------------------------------ 
 *  Comparison of teams
 *)                         
           
let compare_teams r1 r2 =
  match compare (points r2) (points r1) with
  | 0 ->
     begin
       match
         compare ((goals_for r2) - (goals_against r2))
           ((goals_for r1) - (goals_against r1)) with
       | 0 -> compare (goals_for r2) (goals_for r1)
       | n -> n
     end
  | n -> n


(* ------------------------------------------------------------------------
 *  table_and_scorers : 
 *   (team * string list * team * string list) list ->
 *      (team * int * int * int * int * int * int * int) list *
 *      (string * team * int) list
 * ------------------------------------------------------------------------ 
 *  Generate the table and the ordered list of scorers 
 *  from the tournament data
 *)

let table_and_scorers data =
  let drop_last (t,g,w,d,l,gf,ga,p,_) = (t,g,w,d,l,gf,ga,p) in
  let record_list = List.sort compare_teams (data_to_record_list [] data) in
  let table = List.map (fun r -> (drop_last r)) record_list in
  let rec collect_scorers = function
   | [] -> []
   | r :: records ->
      (List.map (fun (x,y) -> (x,(team r),y))
         (scorers_list r)) :: collect_scorers records in
  let scorers =
    List.sort compare_scorers (List.concat (collect_scorers record_list)) in 
  (table, scorers)



(* -----------------------------------------------------------------------------
 *  TESTING: Some simple tests for the functions you have to implement.
 *  If any of them says that the test did not succeed, please see
 *  the returned line numbers and check again your solution.
 *  The tests should not be modified.
 *
 *  During the implementation you may want to comment out tests for
 *  those functions that are not yet implemented.
 *
 *  You can run testing_all () to test all the functions at once.
 *
 *  You can see https://ocaml.org/api/Stdlib.html for the functions
 *  used in the tests.
 *
 * ----------------------------------------------------------------------------
 *)
   
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
          
let testing_unzip () =
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


let testing_table_and_scorers () =
  let l =
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
 

let testing_all () =
  let football_l = testing_table_and_scorers () in
  let count_occurrences_l = testing_count_occurrences () in
  let member_l = testing_member () in 
  let drop_last_l = testing_drop_last () in
  let drop_last_opt_l = testing_drop_last_opt () in
  let zip_with_l = testing_zip_with () in
  let unzip_l = testing_unzip () in
  let l = football_l @ count_occurrences_l @ member_l @ drop_last_l @ drop_last_opt_l @ zip_with_l @ unzip_l in
  if l = [] then (true, l) else (false, l)
