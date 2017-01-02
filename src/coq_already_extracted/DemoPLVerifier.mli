open ASCond
open BinNums
open Datatypes
open Debugging
open DomainGCL
open ImpureConfig
open NumC
open PedraZ
open PredTrans
open ProgVar

type __ = Obj.t

type statement =
| Assume of ZCond.cond
| Assert of char list * ZCond.cond
| Assign of positive * ZCond.Term.term
| Guassign of positive * ZCond.cond
| Seq of statement * statement
| Ifs of ZCond.cond * statement * statement
| While of ZCond.cond * statement * ZCond.cond

val statement_rect :
  (ZCond.cond -> 'a1) -> (char list -> ZCond.cond -> 'a1) -> (positive ->
  ZCond.Term.term -> 'a1) -> (positive -> ZCond.cond -> 'a1) -> (statement ->
  'a1 -> statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 ->
  statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 -> ZCond.cond
  -> 'a1) -> statement -> 'a1

val statement_rec :
  (ZCond.cond -> 'a1) -> (char list -> ZCond.cond -> 'a1) -> (positive ->
  ZCond.Term.term -> 'a1) -> (positive -> ZCond.cond -> 'a1) -> (statement ->
  'a1 -> statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 ->
  statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 -> ZCond.cond
  -> 'a1) -> statement -> 'a1

val sub : ZCond.Term.term -> ZCond.Term.term -> ZCond.Term.term

module G :
 sig
  module DAlarm :
   sig
    type t = FullDom.t

    val isIncl : FullDom.t -> FullDom.t -> bool CoreAlarm.Base.imp

    val top : FullDom.t

    val join : t -> t -> t CoreAlarm.Base.imp

    val widen : t -> t -> t CoreAlarm.Base.imp

    val bottom : FullDom.t

    val isBottom : t -> bool CoreAlarm.Base.imp

    val project : t -> PVar.t -> t CoreAlarm.Base.imp

    val assume : ZCond.t -> FullDom.t -> FullDom.t CoreAlarm.Base.imp

    val coq_assert : ZCond.t -> FullDom.t -> bool CoreAlarm.Base.imp

    val assign :
      PVar.t -> ZCond.Term.t -> FullDom.t -> FullDom.t CoreAlarm.Base.imp

    val guassign :
      PVar.t -> ZCond.t -> FullDom.t -> FullDom.t CoreAlarm.Base.imp
   end

  type cdac =
    DAlarm.t -> DAlarm.t CoreAlarm.Base.imp
    (* singleton inductive, whose constructor was raw_build_cdac *)

  val impl : cdac -> DAlarm.t -> DAlarm.t CoreAlarm.Base.imp

  val build_cdac :
    (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> (ZNum.t Mem.t -> ZNum.t
    Mem.t MPP_Definitions.coq_MPP) -> cdac

  val spec : cdac -> ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP

  val cast :
    cdac -> (ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP) -> cdac

  val skip : cdac

  val fail : char list -> cdac

  val seq : cdac -> cdac -> cdac

  val join : cdac -> cdac -> cdac

  val loop : cdac -> (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> cdac

  val skip_info : char list

  val coq_try : 'a1 option -> ('a1 -> cdac) -> cdac

  val bind :
    (DAlarm.t -> 'a1 CoreAlarm.Base.imp) -> ('a1 -> cdac) -> __ -> cdac

  val assume : ZCond.t -> cdac

  val assert_msg : char list

  val coq_assert : char list -> ZCond.t -> cdac

  val assign : PVar.t -> ZCond.Term.t -> cdac

  val guassign : PVar.t -> ZCond.t -> cdac
 end

val embed : ZCond.t -> FullDom.t CoreAlarm.Base.imp

val postfinder : statement -> G.cdac

val postfinderX : statement -> G.cdac

val verifier : statement -> bool Core.Base.imp
