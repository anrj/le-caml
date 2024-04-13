let total_credit = ref 0
let presum = ref [] 

let add ~grade:(gr1 : float) ~credit:(cr : int) =
      if cr > 0 && gr1 >= 0. && gr1 <= 100. then begin
      let gr = Float.round gr1 in 
      let gp = match gr with
      | gr when 94. <= gr && gr <= 100. -> 4.0
      | gr when 91. <= gr && gr <= 93. -> 3.7
      | gr when 88. <= gr && gr <= 90. -> 3.4
      | gr when 85. <= gr && gr <= 87. -> 3.1
      | gr when 81. <= gr && gr <= 84. -> 2.8
      | gr when 78. <= gr && gr <= 80. -> 2.5
      | gr when 74. <= gr && gr <= 77. -> 2.2
      | gr when 71. <= gr && gr <= 73. -> 1.9
      | gr when 68. <= gr && gr <= 70. -> 1.6
      | gr when 64. <= gr && gr <= 67. -> 1.3
      | gr when 61. <= gr && gr <= 63. -> 1.0
      | gr when 56. <= gr && gr <= 60. -> 0.8
      | gr when 51. <= gr && gr <= 55. -> 0.5
      | _ -> 0.0 in
      total_credit := !total_credit + cr;
      presum := (gp *. float_of_int cr) :: !presum
      end else raise (Invalid_argument "Grade/Credit is not valid")
let calculate () =
   let rec sum_list ?(acc=0.) lst=
   match lst with 
   | [] -> acc
   | h :: t -> sum_list ~acc:(acc +. h) t
in
let sum = sum_list !presum in
Printf.printf "Your GPA: %.2f\n" (sum /. float_of_int !total_credit)
