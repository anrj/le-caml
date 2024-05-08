module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)          
  end

module type GaussianRationalField =
  sig
    include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
    exception Division_by_zero of string
    val from_rational : (int * int ) -> t   (* rational to complex *)     
    val conj : t -> t                       (* conjugate *)
    val re : t -> (int * int)               (* real part *)
    val im : t -> (int * int)               (* imaginary part *)
  end


  module Rationals : RationalField =
  struct
    type t = int * int
    exception Bad_rational of string
    let zero = (0,1)
    let one = (1,1)
    let rec gcd a b =
      if b = 0 then a else gcd b (a mod b)                                            
    let reduce (n,d) =
      if d = 0 then raise (Bad_rational "zero in denominator")
      else let g = gcd n d in (n/g, d/g)
    let standard_form (n,d) =
      let (n1,d1) = reduce (n,d) in
      if n1 < 0 && d1 < 0 then (abs n1, abs d1)
      else if n1 > 0 && d1 < 0 then (-n1, abs d1)
      else if n1 = 0 then (0,1)
      else (n1, d1) 
    let compare (n1,d1) (n2,d2) =
      let (m1,k1) = standard_form (n1,d1) in
      let (m2,k2) = standard_form (n2,d2) in
      Stdlib.compare (m1*k2) (m2*k1)
    let to_string (n,d) =
      let (n1,d1) = standard_form (n,d) in
      if d1=0 then raise (Bad_rational "zero in denominator")
      else if n1=0 then string_of_int 0
      else if d1=1 then string_of_int n1
      else string_of_int n1 ^ "/" ^ string_of_int d1
    let to_float (n,d) = float_of_int n /. float_of_int d
    let from_int i = (i,1)
    let add (n1,d1) (n2,d2) = standard_form (n1*d2 + n2*d1, d1*d2)
    let mul (n1,d1) (n2,d2) = standard_form (n1*n2, d1*d2)
    let sub (n1,d1) (n2,d2) = add (n1,d1) (-n2,d2)
    let div (n1,d1) (n2,d2) = mul (n1,d1) (d2,n2)
    let add_inv (n,d) = standard_form (-n,d)
    let mul_inv (n,d) =
      if d = 0 then raise (Bad_rational "zero in denominator") else standard_form (d,n)
  end


(*
module GaussianRationals : GaussianRationalField =
  struct
    type t = (int  int)  (int * int)
    exception Division_by_zero of string
    let zero = (Rationals.zero, Rationals.zero)
    let one = (Rationals.one, Rationals.zero)
    let compare (r1,i1) (r2,i2) =
      let c = Rationals.compare r1 r2 in
      if c = 0 then Rationals.compare i1 i2 else c
    let to_string (r,i) =
      let (rnum,rden) =  Rationals.standard_form r in
      let (inum,iden) = Rationals.standard_form i in
      let istr = if inum > 0 then "+" ^ Rationals.to_string (inum,iden) ^ "*I"
                 else if inum = 0 then ""
                 else "-" ^ Rationals.to_string (-inum,iden) ^ "*I" in
      let str = if rnum = 0 then istr else Rationals.to_string (rnum,rden) ^ istr in
      str
    let from_rational r = (Rationals.standard_form r, Rationals.zero)
    let add (r1,i1) (r2,i2) = (Rationals.add r1 r2, Rationals.add i1 i2)
    let mul (r1,i1) (r2,i2) =
      (Rationals.sub (Rationals.mul r1 r2) (Rationals.mul i1 i2),
       Rationals.add (Rationals.mul r1 i2) (Rationals.mul r2 i1))
    let sub (r1,i1) (r2,i2) = (Rationals.sub r1 r2, Rationals.sub i1 i2)
    let add_inv (r,i) = (Rationals.add_inv r, Rationals.add_inv i) 
    let mul_inv (r,i) =
      if (Rationals.standard_form r, Rationals.standard_form i) = zero
      then raise (Division_by_zero "")
      else let d = Rationals.add (Rationals.mul r r) (Rationals.mul i i) in
           (Rationals.div r d, Rationals.add_inv (Rationals.div i d))
    let div (r1,i1) (r2,i2) = mul (r1,i1) (mul_inv (r2,i2)) 
    let re (r,_) = Rationals.standard_form r
    let im (_,i) = Rationals.standard_form i
    let conj (r,i) =
      (Rationals.standard_form r, Rationals.standard_form (Rationals.add_inv i))
  end
  *)