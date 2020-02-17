open BinInt
open BinNums
open CoqAddOn
open QArith_base
open Qcanon
open Qround
open Specif
open ZArith_dec

type __ = Obj.t

type trivialMulDiscr =
| IsZero
| IsUnit
| IsOppUnit
| Other

type cmpT =
| EqT
| LeT
| LtT

type cmpG =
| Eq
| Le
| Lt
| Neq

val cmpG2T : cmpG -> cmpT option

val cmpT_eq : cmpT -> cmpT -> bool

module type NumSig =
 sig
  type t

  val z : t

  val u : t

  val add : t -> t -> t

  val mul : t -> t -> t

  val opp : t -> t

  val sub : t -> t -> t

  val eqDec : t -> t -> bool

  val ltLeDec : t -> t -> bool

  val dec : t -> t -> bool sumor

  val isZero : t -> bool

  val mulDiscr : t -> trivialMulDiscr

  val pr : t -> char list

  val prRaw : t -> char list

  val cmpDenote_dec : cmpG -> t -> t -> bool
 end

module QNum :
 sig
  type t = coq_Qc

  val z : t

  val u : t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val opp : t -> t

  val inv : t -> t

  val eqDec : t -> t -> bool

  val ltLeDec : t -> t -> bool

  val dec : t -> t -> bool sumor

  val isZero : t -> bool

  val mulDiscr : t -> trivialMulDiscr

  val pr : t -> char list

  val prRaw : t -> char list

  val to_Q : t -> coq_Q

  val cmpDenote_dec : cmpG -> t -> t -> bool
 end

module ZNum :
 sig
  type t = coq_Z

  val z : t

  val u : t

  val eqDec : t -> t -> bool

  val ltLeDec : t -> t -> bool

  val dec : t -> t -> bool sumor

  val add : t -> t -> t

  val mul : t -> t -> t

  val opp : t -> t

  val pr : t -> char list

  val prRaw : t -> char list

  val sub : t -> t -> t

  val isZero : t -> bool

  val mulDiscr : t -> trivialMulDiscr

  val cmpDenote_dec : cmpG -> t -> t -> bool
 end

module ZtoQ :
 sig
  val ofZ : ZNum.t -> QNum.t

  val isInZ : QNum.t -> __ option

  val floor : QNum.t -> ZNum.t

  val ceil : QNum.t -> ZNum.t
 end
