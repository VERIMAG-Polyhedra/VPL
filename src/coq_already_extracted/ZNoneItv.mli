open BinInt
open BinNums
open Debugging
open DomainInterfaces
open Itv
open LinTerm
open NumC
open ZNone

module ZNItv :
 sig
  type itv = { low : ZN.t; up : ZN.t }

  val low : itv -> ZN.t

  val up : itv -> ZN.t

  type t = itv

  val top : itv

  val fromBndT : ZItv.bndT -> ZN.t

  val fromZItv : ZItv.t -> itv

  val fromQItv : QItv.t -> itv

  val single : coq_Z -> itv

  val select : mode -> ZN.t -> ZN.t -> itv

  val add : mode -> itv -> itv -> itv

  val opp : itv -> itv

  val oppMode : mode -> mode

  val join : mode -> itv -> itv -> itv

  val mulZ : mode -> coq_Z -> itv -> itv

  val mulZZ : mode -> coq_Z -> coq_Z -> itv -> itv

  val mulNN : itv -> itv

  val bndLow : coq_Z -> itv

  val bndUp : coq_Z -> itv

  val isLOW : mode -> bool

  val isUP : mode -> bool

  val mulZNNZ : mode -> coq_Z -> coq_Z -> itv

  val mulZNZN : mode -> coq_Z -> coq_Z -> itv

  val mulNZNZ : mode -> coq_Z -> coq_Z -> itv

  val mul : mode -> itv -> itv -> itv
 end

module NA :
 sig
  type t = ZAffTerm.t option

  val cte : ZN.t -> t

  val add : t -> t -> t

  val opp : t -> t

  val mul : ZN.t -> ZAffTerm.t -> t

  val mulZ1 : ZNum.t -> t -> t

  val mulZ : ZNum.t -> t -> t
 end

module NAItv :
 sig
  type itv = { low : NA.t; up : NA.t }

  val low : itv -> NA.t

  val up : itv -> NA.t

  val cte : ZNItv.t -> itv

  val single : ZAffTerm.t -> itv

  val select : mode -> NA.t -> NA.t -> itv

  val add : mode -> itv -> itv -> itv

  val opp : mode -> itv -> itv

  val mulZ : mode -> coq_Z -> itv -> itv

  val mulN : mode -> ZNItv.t -> ZAffTerm.t -> itv

  val mulP1 : mode -> ZNItv.t -> ZAffTerm.t -> itv
 end
