open ASAtomicCond
open ASCond
open ASTerm
open BinInt
open BinNums
open CstrC
open Datatypes
open Debugging
open DomainInterfaces
open FMapPositive
open ImpureConfig
open Itv
open LinTerm
open LinearizeBackend
open MSetPositive
open NumC
open PredTrans
open ProgVar
open Ring_polynom_AddOn
open String0
open ZNone
open ZNoneItv

type __ = Obj.t

module MakeFull :
 functor (N:NumSig) ->
 functor (Cond:sig
  module Term :
   sig
    module Annot :
     sig
      type topLevelAnnot = TopLevelAnnot.topLevelAnnot =
      | OLD
      | AFFINE
      | INTERV
      | STATIC
      | SKIP_ORACLE

      val topLevelAnnot_rect :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      val topLevelAnnot_rec :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      type t = topLevelAnnot

      val pr : topLevelAnnot -> char list
     end

    type term =
    | Var of PVar.t
    | Cte of N.t
    | Add of term * term
    | Opp of term
    | Mul of term * term
    | Annot of Annot.topLevelAnnot * term

    val term_rect :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    val term_rec :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    type t = term

    val eval : term -> N.t Mem.t -> N.t

    val mdBound : term -> PVar.t -> PVar.t

    val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1

    val map : term -> PVar.t Mem.t -> term

    val pseudoIsZero : term -> bool

    val smartScalAdd1 : N.t -> term -> term

    val smartScalAdd : N.t -> term -> term

    val smartAdd : term -> term -> term

    val smartOpp : term -> term

    val smartScalMul1 : N.t -> term -> term

    val smartScalMul : N.t -> term -> term

    val smartMul : term -> term -> term

    val smartAnnot : Annot.topLevelAnnot -> term -> term

    val import_acc : (PVar.t*N.t) list -> term -> term

    val import : (PVar.t*N.t) list -> term

    val coq_Old : term -> term

    val xeval : term -> N.t Mem.t -> N.t Mem.t -> N.t

    val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term

    val isCte : term -> bool

    val annotAFFINEx : term -> term

    val annotAFFINE_rec : term -> term option

    val annotAFFINE : term -> term

    val matchCte : term -> N.t option

    val pr : term -> char list
   end

  type cond =
  | Basic of bool
  | Atom of cmpG * Term.term * Term.term
  | BinL of binl * cond * cond
  | Not of cond

  val cond_rect :
    (bool -> 'a1) -> (cmpG -> Term.term -> Term.term -> 'a1) -> (binl -> cond
    -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  val cond_rec :
    (bool -> 'a1) -> (cmpG -> Term.term -> Term.term -> 'a1) -> (binl -> cond
    -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  type t = cond

  val sat_dec : t -> N.t Mem.t -> bool

  val xsat_dec : t -> N.t Mem.t -> N.t Mem.t -> bool

  val mdBound : cond -> positive -> positive

  val map : cond -> PVar.t Mem.t -> cond

  val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond

  val dual : cmpG -> Term.term -> Term.term -> t

  val nnf : t -> t

  val nnfNot : t -> t
 end) ->
 functor (I:sig
  type t
 end) ->
 functor (AtomC:sig
  type atomicCond = { cmpOp : cmpG; right : Cond.Term.t }

  val cmpOp : atomicCond -> cmpG

  val right : atomicCond -> Cond.Term.t

  type t = atomicCond

  val make : Cond.Term.term -> cmpG -> Cond.Term.term -> t
 end) ->
 functor (D:sig
  type t

  val isIncl : t -> t -> bool Core.Base.imp

  val top : t

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp
 end) ->
 functor (AtomD:sig
  val assume : AtomC.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (R:sig
  val rename : PVar.t -> PVar.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (DI:sig
  val getItvMode : mode -> Cond.Term.t -> D.t -> I.t Core.Base.imp
 end) ->
 functor (DP:sig
  val pr : D.t -> char list

  val to_string : (PVar.t -> char list) -> D.t -> char list

  type rep

  val backend_rep :
    D.t -> (rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

  val meet : D.t -> D.t -> D.t Core.Base.imp
 end) ->
 sig
  module Dom :
   sig
    type t = D.t

    val isIncl : t -> t -> bool Core.Base.imp

    val top : t

    val join : t -> t -> t Core.Base.imp

    val widen : t -> t -> t Core.Base.imp

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val skipBottom : D.t -> D.t Core.Base.imp -> D.t Core.Base.imp

    val assumeRec : Cond.t -> D.t -> D.t Core.Base.imp

    val assume : Cond.cond -> D.t -> D.t Core.Base.imp

    module Naive :
     sig
      val coq_assert : Cond.cond -> t -> bool Core.Base.imp
     end

    val assertRec : Cond.t -> t -> bool Core.Base.imp

    val coq_assert : Cond.t -> t -> bool Core.Base.imp
   end

  val encode : bool -> positive -> positive

  val decode : positive -> positive

  val encodeE : PositiveSet.t -> positive -> positive

  val encodeO : PositiveSet.t -> positive -> positive

  val decodeIn : PositiveSet.t -> N.t Mem.t -> N.t Mem.t -> positive -> N.t

  val switch : PositiveSet.t -> positive -> PositiveSet.t

  type assignDomain = { pol : Dom.t; renaming : PositiveSet.t }

  val pol : assignDomain -> Dom.t

  val renaming : assignDomain -> PositiveSet.t

  type t = assignDomain

  val top : t

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val assume : Cond.cond -> t -> t Core.Base.imp

  val coq_assert : Cond.cond -> t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp

  val guassignAux : positive -> Cond.cond -> t -> t Core.Base.imp

  val guassign : PVar.t -> Cond.cond -> t -> t Core.Base.imp

  val assign : positive -> Cond.Term.term -> t -> t Core.Base.imp

  val nop : PVar.t -> assignDomain -> t Core.Base.imp

  val switchAll : PositiveSet.t -> t -> t Core.Base.imp

  val isIncl : t -> t -> bool Core.Base.imp

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val rename :
    positive -> positive -> assignDomain -> assignDomain Core.Base.imp

  val getItvMode : mode -> Cond.Term.t -> t -> I.t Core.Base.imp

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  type rep = DP.rep

  val backend_rep :
    t -> (DP.rep*((PVar.t -> positive)*(positive -> PVar.t))) option

  val meet : t -> t -> t Core.Base.imp
 end

module MakeZ :
 functor (D:sig
  type t

  val isIncl : t -> t -> bool Core.Base.imp

  val top : t

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp
 end) ->
 functor (QCstrD:sig
  val assume : Cstr.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (QItvD:sig
  val getItvMode : mode -> QAffTerm.t -> D.t -> QItv.t Core.Base.imp
 end) ->
 functor (R:sig
  val rename : PVar.t -> PVar.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (DP:sig
  val pr : D.t -> char list

  val to_string : (PVar.t -> char list) -> D.t -> char list

  type rep

  val backend_rep :
    D.t -> (rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

  val meet : D.t -> D.t -> D.t Core.Base.imp
 end) ->
 sig
  module BasicD :
   sig
    type t = D.t

    val isIncl : D.t -> D.t -> bool Core.Base.imp

    val top : D.t

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val widen : D.t -> D.t -> D.t Core.Base.imp

    val join : D.t -> D.t -> D.t Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp

    val pr : t -> char list

    val to_string : (PVar.t -> char list) -> D.t -> char list

    type rep = DP.rep

    val backend_rep :
      D.t -> (DP.rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

    val meet : D.t -> D.t -> D.t Core.Base.imp
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
    type t = D.t

    val isIncl : D.t -> D.t -> bool Core.Base.imp

    val top : D.t

    val bottom : t

    val isBottom : t -> bool Core.Base.imp

    val widen : D.t -> D.t -> D.t Core.Base.imp

    val join : D.t -> D.t -> D.t Core.Base.imp

    val project : t -> PVar.t -> t Core.Base.imp

    val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp

    val pr : t -> char list

    val to_string : (PVar.t -> char list) -> D.t -> char list

    type rep = DP.rep

    val backend_rep :
      D.t -> (DP.rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

    val meet : D.t -> D.t -> D.t Core.Base.imp

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
