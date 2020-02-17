open ASAtomicCond
open ASCond
open ASTerm
open BinNums
open DomainFunctors
open DomainInterfaces
open FMapPositive
open ImpureConfig
open LinTerm
open MSetPositive
open NumC
open PedraQ
open PredTrans
open ProgVar
open Ring_polynom_AddOn
open ZNoneItv

type __ = Obj.t

module FullDom :
 sig
  module BasicD :
   sig
    type t = BasicD.t

    val isIncl : BasicD.t -> BasicD.t -> bool Core.Base.imp

    val top : BasicD.t

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val widen : BasicD.t -> BasicD.t -> BasicD.t Core.Base.imp

    val join : BasicD.t -> BasicD.t -> BasicD.t Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp

    val pr : t -> char list

    val to_string : (PVar.t -> char list) -> BasicD.t -> char list

    type rep = BasicD.rep

    val backend_rep :
      BasicD.t -> (BasicD.rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

    val meet : BasicD.t -> BasicD.t -> BasicD.t Core.Base.imp
   end

  module ZNItvD :
   sig
    val getItvMode :
      mode -> BasicZTerm.term -> BasicD.t -> ZNItv.itv Core.Base.imp
   end

  module CstrD :
   sig
    val assume : ZtoQCstr.t -> BasicD.t -> BasicD.t Core.Base.imp
   end

  module AtomicD :
   sig
    type t = PedraQ.BasicD.t

    val isIncl : PedraQ.BasicD.t -> PedraQ.BasicD.t -> bool Core.Base.imp

    val top : PedraQ.BasicD.t

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val widen :
      PedraQ.BasicD.t -> PedraQ.BasicD.t -> PedraQ.BasicD.t Core.Base.imp

    val join :
      PedraQ.BasicD.t -> PedraQ.BasicD.t -> PedraQ.BasicD.t Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp

    val pr : t -> char list

    val to_string : (PVar.t -> char list) -> PedraQ.BasicD.t -> char list

    type rep = PedraQ.BasicD.rep

    val backend_rep :
      PedraQ.BasicD.t -> (PedraQ.BasicD.rep*((PVar.t -> PVar.t)*(PVar.t ->
      PVar.t))) option

    val meet :
      PedraQ.BasicD.t -> PedraQ.BasicD.t -> PedraQ.BasicD.t Core.Base.imp

    val affAssumeLe : ZAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp

    val affAssumeLt : ZAffTerm.affTerm -> BasicD.t -> BasicD.t Core.Base.imp

    val affAssumeGt : ZAffTerm.affTerm -> BasicD.t -> BasicD.t Core.Base.imp

    val affAssume : cmpG -> ZAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp

    val add_variable :
      BasicD.t -> PVar.t -> ZNItv.t PositiveMap.t -> ZNItv.t PositiveMap.t
      Core.Base.imp

    val get_variables :
      BasicD.t -> ZTerm.term -> ZNItv.t PositiveMap.t Core.Base.imp

    val mitv_find : ZNItv.t PositiveMap.t -> PositiveMap.key -> ZNItv.t

    val intervalize :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> BasicD.t ->
      ZNItv.t Core.Base.imp

    module G :
     sig
      type cdac =
        BasicD.t -> BasicD.t Core.Base.imp
        (* singleton inductive, whose constructor was raw_build_cdac *)

      val impl : cdac -> BasicD.t -> BasicD.t Core.Base.imp

      val build_cdac :
        (BasicD.t -> BasicD.t Core.Base.imp) -> (ZNum.t Mem.t -> ZNum.t Mem.t
        MPP_Definitions.coq_MPP) -> cdac

      val spec : cdac -> ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP

      val cast :
        cdac -> (ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP) -> cdac

      val skip : cdac

      val fail : char list -> cdac

      val seq : cdac -> cdac -> cdac

      val join : cdac -> cdac -> cdac

      val loop : cdac -> (BasicD.t -> BasicD.t Core.Base.imp) -> cdac

      val skip_info : char list

      val coq_try : 'a1 option -> ('a1 -> cdac) -> cdac

      val bind :
        (BasicD.t -> 'a1 Core.Base.imp) -> ('a1 -> cdac) -> __ -> cdac
     end

    val gAffAssumeLe : ZAffTerm.t -> G.cdac

    val gAffAssumeLt : ZAffTerm.affTerm -> G.cdac

    val gAffAssumeGt : ZAffTerm.affTerm -> G.cdac

    val gIntervalize :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (ZNItv.t ->
      G.cdac) -> G.cdac

    val intervalizeG :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.t -> (NAItv.itv -> G.cdac)
      -> G.cdac

    val castAFFINE_error : char list

    val castAFFINE : BasicZTerm.term -> (ZAffTerm.t -> G.cdac) -> G.cdac

    val caseSign :
      BasicZTerm.term -> (ZAffTerm.t -> G.cdac) -> (ZAffTerm.t -> G.cdac) ->
      G.cdac

    val linearizeMul :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> ZTerm.term ->
      (NAItv.itv -> G.cdac) -> G.cdac

    val linearizeG :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (NAItv.itv ->
      G.cdac) -> G.cdac

    val linearizeGX :
      (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (NAItv.itv ->
      G.cdac) -> G.cdac

    val assumeOpCPS :
      (PVar.t -> ZNItv.t) -> bool -> cmpG -> ZTerm.term -> ZAffTerm.affTerm
      -> G.cdac

    val assumeOpAnnot :
      (PVar.t -> ZNItv.t) -> bool -> cmpG -> ZTerm.term -> ZAffTerm.affTerm
      -> G.cdac

    module ZPeq :
     sig
      module M1 :
       sig
        val toPExpr : ZTerm.term -> coq_PExpr
       end

      module M2 :
       sig
        val toPExpr : ZTerm.term -> coq_PExpr
       end

      val pomial_eq : ZTerm.t -> ZTerm.t -> bool
     end

    val test_eq : ZTerm.t -> ZTerm.t -> ZTerm.t

    val skip_oracle : ZTerm.t -> bool

    val assumeOpFromOracle :
      (PVar.t -> ZNItv.t) -> bool -> linearizeContext -> cmpG -> ZTerm.t ->
      ZAffTerm.affTerm -> G.cdac

    val assumeOp2 :
      (PVar.t -> ZNItv.t) -> bool -> linearizeContext -> cmpG -> ZTerm.term
      -> ZAffTerm.affTerm -> G.cdac

    val assumeOp :
      bool -> cmpG -> ZTerm.t -> ZAffTerm.t -> ZTerm.t -> BasicD.t ->
      BasicD.t Core.Base.imp

    val assume : ZAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp

    val getItvMode : mode -> ZTerm.term -> BasicD.t -> ZNItv.t Core.Base.imp
   end

  module Dom :
   sig
    type t = AtomicD.t

    val isIncl : t -> t -> bool Core.Base.imp

    val top : t

    val join : t -> t -> t Core.Base.imp

    val widen : t -> t -> t Core.Base.imp

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val skipBottom :
      AtomicD.t -> AtomicD.t Core.Base.imp -> AtomicD.t Core.Base.imp

    val assumeRec : ZCond.t -> AtomicD.t -> AtomicD.t Core.Base.imp

    val assume : ZCond.cond -> AtomicD.t -> AtomicD.t Core.Base.imp

    module Naive :
     sig
      val coq_assert : ZCond.cond -> t -> bool Core.Base.imp
     end

    val assertRec : ZCond.t -> t -> bool Core.Base.imp

    val coq_assert : ZCond.t -> t -> bool Core.Base.imp
   end

  val encode : bool -> positive -> positive

  val decode : positive -> positive

  val encodeE : PositiveSet.t -> positive -> positive

  val encodeO : PositiveSet.t -> positive -> positive

  val decodeIn :
    PositiveSet.t -> ZNum.t Mem.t -> ZNum.t Mem.t -> positive -> ZNum.t

  val switch : PositiveSet.t -> positive -> PositiveSet.t

  type assignDomain = { pol : Dom.t; renaming : PositiveSet.t }

  val pol : assignDomain -> Dom.t

  val renaming : assignDomain -> PositiveSet.t

  type t = assignDomain

  val top : t

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val assume : ZCond.cond -> t -> t Core.Base.imp

  val coq_assert : ZCond.cond -> t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp

  val guassignAux : positive -> ZCond.cond -> t -> t Core.Base.imp

  val guassign : PVar.t -> ZCond.cond -> t -> t Core.Base.imp

  val assign : positive -> ZCond.Term.term -> t -> t Core.Base.imp

  val nop : PVar.t -> assignDomain -> t Core.Base.imp

  val switchAll : PositiveSet.t -> t -> t Core.Base.imp

  val isIncl : t -> t -> bool Core.Base.imp

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val rename :
    positive -> positive -> assignDomain -> assignDomain Core.Base.imp

  val getItvMode : mode -> ZCond.Term.t -> t -> ZNItv.t Core.Base.imp

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  type rep = AtomicD.rep

  val backend_rep :
    t -> (AtomicD.rep*((PVar.t -> positive)*(positive -> PVar.t))) option

  val meet : t -> t -> t Core.Base.imp
 end
