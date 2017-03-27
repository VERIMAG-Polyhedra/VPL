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

module Index :
 functor (N:Num) ->
 sig
  type t = N.t list

  val eq_dec : t -> t -> bool

  val compare : t -> t -> t OrderedType.coq_Compare
 end

module NatNum :
 sig
  type t = nat

  val z : nat

  val eq_dec : nat -> nat -> bool

  val lt_eq_dec : t -> t -> bool sumor
 end

module QcNum :
 sig
  type t = coq_Qc

  val z : coq_Qc

  val eq_dec : t -> t -> bool

  val lt_eq_dec : t -> t -> bool sumor
 end

module NatIndex :
 sig
  type t = NatNum.t list

  val eq_dec : t -> t -> bool

  val compare : t -> t -> t OrderedType.coq_Compare
 end

module QcIndex :
 sig
  type t = QcNum.t list

  val eq_dec : t -> t -> bool

  val compare : t -> t -> t OrderedType.coq_Compare
 end
