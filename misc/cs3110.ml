(*Modullar programming exercises*)

module type ComplexSig = sig
  type t = float * float
  val zero : t
  val add : t -> t -> t
end

module Complex : ComplexSig = struct
  type t = float * float
  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end

(** Creates a ListQueue filled with [n] elements. *)

(*ListQueue module*)
module ListQueue = struct
  let empty = []
  let enqueue t q = q @ [t]
  let dequeue q = List.hd q

end

let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

  module type Fraction = sig
    (* A fraction is a rational number p/q, where q != 0. *)
    type t
  
    (** [make n d] is n/d. Requires d != 0. *)
    val make : int -> int -> t
  
    val numerator : t -> int
    val denominator : t -> int
    val to_string : t -> string
    val to_float : t -> float
  
    val add : t -> t -> t
    val mul : t -> t -> t
  end

  module Frac : Fraction= struct
    type t = int * int
    let make n d = if d <> 0 then n, d else raise Division_by_zero
    let numerator ((n, d) : t )= n
    let denominator ((n, d) : t) = d
    let to_string ((n, d) : t) = string_of_int n ^ "/" ^ string_of_int d
    let to_float ((n,d) : t) = float_of_int n /. float_of_int d
    let add ((a1, b1) : t) ((a2, b2) : t) =  if b1 mod b2 = 0 then (a1 + a2 * (b1/b2)), b1 else if b2 mod b1 = 0 then (a1 * (b2/b1) + a2), b2 else (a1*b2 + a2 * b1), b1*b2
    let mul (a1, b1) (a2, b2) = a1 * a2, b1 * b2
  end

  (*Coming back to this before finals*)