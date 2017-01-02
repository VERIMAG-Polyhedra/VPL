open ASAtomicCond
open ASCond
open ASTerm
open BinNums
open ConsSet
open CoqAddOn
open CstrC
open CstrLCF
open Datatypes
open Debugging
open DomainFunctors
open DomainInterfaces
open ImpureConfig
open Itv
open LinTerm
open LinearizeBackend
open List0
open MSetPositive
open Map_poly
open NumC
open PedraQBackend
open ProgVar
open QArith_base
open Qcanon
open String0

module BasicD :
 sig
  type polT = { cons : Cs.t; ml : t }

  val cons : polT -> Cs.t

  val ml : polT -> t

  val polPr : polT -> char list

  val pol_to_string : (PVar.t -> char list) -> polT -> char list

  val wrap : polT -> Cs.cstr pedraCert

  val polTop : polT

  type t = polT option

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> polT option -> char list

  val id : PVar.t -> PVar.t

  type rep = PedraQBackend.t

  val backend_rep :
    polT option -> (PedraQBackend.t*((PVar.t -> PVar.t)*(PVar.t -> PVar.t)))
    option

  val top : t

  val my_assert : char list -> bool -> bool

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val isIncl : t -> t -> bool Core.Base.imp

  val join : t -> t -> t Core.Base.imp

  val meet : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp
 end

module LinItvD :
 sig
  val buildLow : BasicD.polT -> LinQ.t -> Cs.cstr bndT -> QItv.bndT

  val buildUp : BasicD.polT -> LinQ.t -> Cs.cstr bndT -> QItv.bndT

  val getItv : BasicD.t -> LinQ.t -> QItv.t Core.Base.imp

  val getLowerBound : BasicD.t -> LinQ.t -> QItv.bndT Core.Base.imp

  val getUpperBound : BasicD.t -> LinQ.t -> QItv.bndT Core.Base.imp

  val getItvMode : mode -> LinQ.t -> BasicD.t -> QItv.t Core.Base.imp
 end

module AffItvD :
 sig
  val getItvMode :
    mode -> QAffTerm.affTerm -> BasicD.t -> QItv.tInd Core.Base.imp
 end

module ItvD :
 sig
  val getItvMode :
    mode -> BasicQTerm.term -> BasicD.t -> QItv.tInd Core.Base.imp

  val get_itv : BasicQTerm.term -> BasicD.t -> QItv.tInd Core.Base.imp
 end

module CstrD :
 sig
  val assume : Cstr.t -> BasicD.t -> BasicD.t Core.Base.imp
 end

module Rename :
 sig
  val rename : PVar.t -> PVar.t -> BasicD.t -> BasicD.t Core.Base.imp
 end

module QAtomicCondAssume :
 sig
  val affAssume : cmpG -> QAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp

  val applyHandelman_one :
    cmpG -> QTerm.t -> BasicD.polT -> BasicD.t -> Handelman_compute.certif ->
    BasicD.t Core.Base.imp

  val f :
    cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif -> BasicD.t
    Core.Base.imp -> BasicD.t Core.Base.imp

  val applyHandelman :
    cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif list ->
    BasicD.t Core.Base.imp

  val assume_eq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp

  val assume_neq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp

  val assume : QAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp
 end

module AtomicD :
 sig
  type polT = BasicD.polT = { cons : Cs.t; ml : t }

  val cons : polT -> Cs.t

  val ml : polT -> t

  val polPr : polT -> char list

  val pol_to_string : (PVar.t -> char list) -> polT -> char list

  val wrap : polT -> Cs.cstr pedraCert

  val polTop : polT

  type t = polT option

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> polT option -> char list

  val id : PVar.t -> PVar.t

  type rep = PedraQBackend.t

  val backend_rep :
    polT option -> (PedraQBackend.t*((PVar.t -> PVar.t)*(PVar.t -> PVar.t)))
    option

  val top : t

  val my_assert : char list -> bool -> bool

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val isIncl : t -> t -> bool Core.Base.imp

  val join : t -> t -> t Core.Base.imp

  val meet : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp

  val affAssume : cmpG -> QAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp

  val applyHandelman_one :
    cmpG -> QTerm.t -> BasicD.polT -> BasicD.t -> Handelman_compute.certif ->
    BasicD.t Core.Base.imp

  val f :
    cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif -> BasicD.t
    Core.Base.imp -> BasicD.t Core.Base.imp

  val applyHandelman :
    cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif list ->
    BasicD.t Core.Base.imp

  val assume_eq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp

  val assume_neq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp

  val assume : QAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp
 end

module FullDom :
 sig
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

    val assumeRec : QCond.t -> AtomicD.t -> AtomicD.t Core.Base.imp

    val assume : QCond.cond -> AtomicD.t -> AtomicD.t Core.Base.imp

    module Naive :
     sig
      val coq_assert : QCond.cond -> t -> bool Core.Base.imp
     end

    val assertRec : QCond.t -> t -> bool Core.Base.imp

    val coq_assert : QCond.t -> t -> bool Core.Base.imp
   end

  val encode : bool -> positive -> positive

  val decode : positive -> positive

  val encodeE : PositiveSet.t -> positive -> positive

  val encodeO : PositiveSet.t -> positive -> positive

  val decodeIn :
    PositiveSet.t -> QNum.t Mem.t -> QNum.t Mem.t -> positive -> QNum.t

  val switch : PositiveSet.t -> positive -> PositiveSet.t

  type assignDomain = { pol : Dom.t; renaming : PositiveSet.t }

  val pol : assignDomain -> Dom.t

  val renaming : assignDomain -> PositiveSet.t

  type t = assignDomain

  val top : t

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val assume : QCond.cond -> t -> t Core.Base.imp

  val coq_assert : QCond.cond -> t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp

  val guassignAux : positive -> QCond.cond -> t -> t Core.Base.imp

  val guassign : PVar.t -> QCond.cond -> t -> t Core.Base.imp

  val assign : positive -> QCond.Term.term -> t -> t Core.Base.imp

  val nop : PVar.t -> assignDomain -> t Core.Base.imp

  val switchAll : PositiveSet.t -> t -> t Core.Base.imp

  val isIncl : t -> t -> bool Core.Base.imp

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val rename :
    positive -> positive -> assignDomain -> assignDomain Core.Base.imp

  val getItvMode : mode -> QCond.Term.t -> t -> QItv.t Core.Base.imp

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  type rep = AtomicD.rep

  val backend_rep :
    t -> (AtomicD.rep*((PVar.t -> positive)*(positive -> PVar.t))) option

  val meet : t -> t -> t Core.Base.imp
 end
