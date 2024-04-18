(* ***************************************************************************** 

  ASSIGNMENT 4. DUE May 7

  
  Number of problems: 6
  Total points: 5

  
  *************************************************************************** *) 

(* Problem 1: 0.8 points.
 *
 * Find functions f1, f2, and f3, such that
 *
 *  fold_left f1 [] [(a1, b1) ; ... ; (an, bn)] for arbitrary ai, bi computes
 *  the list [(b1, a1); ... ; (bn, an) ]
 *
 *  fold_left f2 [] [a_0 ; ... ; a_{n−3} ; a_{n−2}; a_{n−1}; a_n] 
 *  for arbitrary elements a_i computes the list
 *  [a_n; a_{n−2} ; ... ; a_0 ; ... ; a_{n−3} ; a_{n−1}]
 *
 *  fold_left f3 (fun _ -> 0) [(k1 , v1) ; ... ; (kn, vn) ] computes a function 
 *  g such that g(ki) = vi for all 1 ≤ i ≤ n. The k's are assumed to be 
 *  pairwise distinct.
 *
 *  WRITE YOUR IMPLEMENTATIONS OF f1, f2, AND f3.
 *)


(* Problem 2: 0.8 points.
 *
 *  Rewrite the following functions in a tail-recursive form:
 *
 *  let rec map f = function
      | [] -> []
      | x :: xs -> f x :: map f xs
 *
 *  
 *  let rec replicate n x =
      if n < 1 then [] else x :: replicate (n-1) x
 *
 *  Call the tail recursive variants respectively map_tr and replicate_tr
 *)




(* *****************************************************************************

   Definition of lazy lists

***************************************************************************** *)
 
(* -----------------------------------------------------------------------------
 *  'a custom_llist
 * -----------------------------------------------------------------------------
 *  Defines custom lazy lists.
 *)

 type 'a custom_llist = (unit -> 'a custom_cell)
 and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
 
 
 
 
 (* -----------------------------------------------------------------------------
  *  'a ocaml_llist
  * -----------------------------------------------------------------------------
  *  Defines OCaml lazy lists.
  *)
 
 type 'a ocaml_llist = 'a ocaml_cell Lazy.t
 and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)
 
                                       
     
 
 (* Problem 3: 0.8 points
  * -----------------------------------------------------------------------------
  *
  *  Implement a mapping function that maps a function over a lazy list.
  *  Implement it both for custom and OCaml lazy list variants.
  *  Call them respectively map_over_custom_llist and map_over_ocaml_llist.
  *  
  *
  *)
 
 
 
                          
 (* Problem 4: 0.8 points
  * -----------------------------------------------------------------------------
  *
  *  Implement a merging function that combines two sorted lazy lists.
  *   
  *  The idea of merging two lists: 
       merge [1;4;6;7;8; ... ] [1;2;3;4;10; ... ] = [1;1;2;3;4;4;6;7;8;10; ... ]
  *
  *  Implement the function both for custom and OCaml lazy list variants.
  *  Call them respectively merge_custom_llists and merge_ocaml_llists.
  *  
  *
  *)
 
 
 
 (* Problem 5: 0.8 points
  * -----------------------------------------------------------------------------
  *
  *  Implement a function that drops duplicates from a sorted lazy list.
  *
  *  Implement it both for custom and OCaml lazy list variants.
  *  Call them respectively drop_dupl_custom_llist and drop_dupl_ocaml_llist.
  *  
  *
  *)        
 
 
                  
 
 (* Problem 6: 1.0 points
  * -----------------------------------------------------------------------------
  *
  *  Implement a function hamming that lazily computes the infinite sequence of 
  *  Hamming numbers (i.e., all natural numbers whose only prime factors are 
  *  2, 3, and 5), e.g.,
  *    hamming = [1;2;3;4;5;6;8;9;10;12;15;16;18;20; ... ]
  *
  *  Implement it both for custom and OCaml lazy list variants.
  *  Call them respectively hamming_custom_llist and hamming_ocaml_llist.
  *
  *)        
 
 
 
 (* ****************************************************************************
  
    TESTS. You can use them to check your implementation. 
    They should not be modified.
 
 **************************************************************************** *)
 
 
 (* -----------------------------------------------------------------------------
  *  TESTING: Some simple tests for functions f1, f2 and f3. 
  *  If testing_fs () does not succeed, please check the line numbers in the 
  *  returend list to see which test failed, and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)                                    
 
 let testing_fs () =
    let l =
      [
        __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                       [(2,1); (4,3); (6,5)]);
        __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                       ['g';'e';'c';'a';'b';'d';'f']);
        __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                               [('a',3); ('z', -9); ('d', 18)] in
                     (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
     ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
    else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
 
 
 
 (* -----------------------------------------------------------------------------
  *  TESTING: Simple tests for map_tr and replicate_tr. 
  *  If test_tr_llist () says that the test did not succeed
  *  please check the returned line numbers to see which tests failed, 
  *  and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)     
 
 let test_tr_llist () =
   let l =
     [
       __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
       __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
       __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
       __LINE_OF__ (replicate_tr (-3) "a" = [])
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
   else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
                
 (* -----------------------------------------------------------------------------
  *  TESTING: Helper functions used for testing lazy lists
  * -----------------------------------------------------------------------------
  *)    
 
 let rec from_to_custom from to_ step =
       if from <= to_
       then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
       else fun () -> NilC
 
 let rec print_custom_llist n c_list =
   if n != 0
   then match c_list () with
        | NilC -> print_string "Nil\n"
        | ConsC (h, t) ->
           Printf.printf "%d, " h;
           print_custom_llist (n-1) t
   else print_string "...\n"
 
 let rec custom_llist_to_string n c_list =
   if n != 0
   then match c_list () with
     | NilC -> "Nil"
     | ConsC (h, t) ->
        string_of_int h ^ ", " ^
          custom_llist_to_string (n-1) t
   else "..."
 
 let rec from_to_ocaml from to_ step =
       if from <= to_
       then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
       else lazy NilO
 
 let rec print_ocaml_llist n o_list =
   if n != 0
   then match Lazy.force o_list with
     | NilO -> print_string "Nil\n"
     | ConsO (h, t) ->
        Printf.printf "%d, " h;
        print_ocaml_llist (n-1) t
   else print_string "...\n"
 
 let rec ocaml_llist_to_string n o_list =
   if n != 0
   then match Lazy.force o_list with
     | NilO -> "Nil"
     | ConsO (h, t) ->
        string_of_int h ^ ", " ^
          ocaml_llist_to_string (n-1) t
   else "..."
  
 (* -----------------------------------------------------------------------------
  *  TESTING: Simple tests for map_over_custom_llist and map_over_ocaml_llist. 
  *  If test_map_llist () says that the test did not succeed
  *  please check the returned line numbers to see which tests failed, 
  *  and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)     
 
 let test_map_llist () =
   let l =
     [
       __LINE_OF__ (custom_llist_to_string 10
         (map_over_custom_llist (fun x -> x+1) (from_to_custom 0 5 1)) =
                      "1, 2, 3, 4, 5, 6, Nil");
       __LINE_OF__ (custom_llist_to_string 10
         (map_over_custom_llist (fun x -> x+1) (from_to_custom 6 5 1)) =
                      "Nil");
        __LINE_OF__ (ocaml_llist_to_string 10
         (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                       "1, 2, 3, 4, 5, 6, Nil");
         __LINE_OF__ (ocaml_llist_to_string 10
         (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                        "Nil")
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
   else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
 (* -----------------------------------------------------------------------------
  *  TESTING: Simple tests for merge_custom_llists and merge_ocaml_llists. 
  *  If test_merge_llists () says that the test did not succeed
  *  please check the returned line numbers to see which tests failed, 
  *  and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)     
 
 let test_merge_llists () =
   let l =
     [
       __LINE_OF__ (custom_llist_to_string 13
         (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                      "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
       __LINE_OF__ (custom_llist_to_string 13
                      (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                      "0, 1, 2, 3, 4, 5, Nil");
       __LINE_OF__ (ocaml_llist_to_string 13
         (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                      "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
       __LINE_OF__ (ocaml_llist_to_string 13
                      (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                      "0, 1, 2, 3, 4, 5, Nil")
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
   else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
          
 (* -----------------------------------------------------------------------------
  *  TESTING: Simple tests for drop_dupl_custom_llist and drop_dupl_ocaml_llist. 
  *  If test_drop_dupl_llists () says that the test did not succeed
  *  please check the returned line numbers to see which tests failed, 
  *  and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)     
 
 let test_drop_dupl_llists () =
   let l =
     [
       __LINE_OF__ (custom_llist_to_string 13
                      (drop_dupl_custom_llist
                         (merge_custom_llists (from_to_custom 0 5 1)
                            (from_to_custom 0 5 2))) =
                      "0, 1, 2, 3, 4, 5, Nil");
       __LINE_OF__ (custom_llist_to_string 13
                      (drop_dupl_custom_llist
                         (merge_custom_llists (from_to_custom 0 5 1)
                            (from_to_custom 6 5 1))) =
                      "0, 1, 2, 3, 4, 5, Nil");
       __LINE_OF__ (ocaml_llist_to_string 13
                      (drop_dupl_ocaml_llist
                         (merge_ocaml_llists (from_to_ocaml 0 5 1)
                            (from_to_ocaml 0 5 1))) =
                      "0, 1, 2, 3, 4, 5, Nil");
       __LINE_OF__ (ocaml_llist_to_string 13
                      (drop_dupl_ocaml_llist
                         (merge_ocaml_llists (from_to_ocaml 0 5 1)
                            (from_to_ocaml 6 5 1))) =
                      "0, 1, 2, 3, 4, 5, Nil")
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
   else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
 
                                          
  (* -----------------------------------------------------------------------------
  *  TESTING: Simple tests for hamming_custom and hamming_ocaml. 
  *  If test_hamming_llists () says that the test did not succeed
  *  please check the returned line numbers to see which tests failed, 
  *  and then check again your solution.
  *  The tests should not be modified.
  * -----------------------------------------------------------------------------
  *)     
 
 let test_hamming_llists () =
   let l =
     [
       __LINE_OF__ (custom_llist_to_string 14 hamming_custom =
                      "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
       __LINE_OF__ (custom_llist_to_string 20 hamming_custom = 
                      "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
       __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml =
                      "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
       __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml = 
                      "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
   else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
 
   
 