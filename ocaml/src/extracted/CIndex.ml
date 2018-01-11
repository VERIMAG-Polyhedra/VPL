open BinNums
open Compare_dec
open Datatypes
open List0
open PeanoNat
open QArith_base
open Qcanon
open Specif

module type Num =
 sig
  type t

  val z : t

  val eq_dec : t -> t -> bool

  val lt_eq_dec : t -> t -> bool sumor
 end

module Index =
 functor (N:Num) ->
 struct
  type t = N.t list

  (** val eq_dec : t -> t -> bool **)

  let eq_dec x y =
    list_eq_dec N.eq_dec x y

  (** val compare : t -> t -> t OrderedType.coq_Compare **)

  let rec compare x y =
    match x with
    | [] -> (match y with
             | [] -> OrderedType.EQ
             | _::_ -> OrderedType.LT)
    | a::x1 ->
      (match y with
       | [] -> OrderedType.GT
       | b::y1 ->
         if N.eq_dec a b
         then let pf_cons' = compare x1 y1 in
              (match pf_cons' with
               | OrderedType.LT -> OrderedType.LT
               | OrderedType.EQ -> OrderedType.EQ
               | OrderedType.GT -> OrderedType.GT)
         else let dEC = N.lt_eq_dec a b in
              (match dEC with
               | Coq_inleft x0 ->
                 if x0 then OrderedType.LT else assert false (* absurd case *)
               | Coq_inright -> OrderedType.GT))
 end

module NatNum =
 struct
  type t = nat

  (** val z : nat **)

  let z =
    O

  (** val eq_dec : nat -> nat -> bool **)

  let eq_dec =
    Nat.eq_dec

  (** val lt_eq_dec : t -> t -> bool sumor **)

  let lt_eq_dec =
    lt_eq_lt_dec
 end

module QcNum =
 struct
  type t = coq_Qc

  (** val z : coq_Qc **)

  let z =
    coq_Q2Qc { coq_Qnum = Z0; coq_Qden = Coq_xH }

  (** val eq_dec : t -> t -> bool **)

  let eq_dec =
    coq_Qc_eq_dec

  (** val lt_eq_dec : t -> t -> bool sumor **)

  let lt_eq_dec x y =
    let h = coq_Qc_dec x y in
    (match h with
     | Coq_inleft x0 -> if x0 then Coq_inleft true else Coq_inright
     | Coq_inright -> Coq_inleft false)
 end

module NatIndex = Index(NatNum)

module QcIndex = Index(QcNum)
