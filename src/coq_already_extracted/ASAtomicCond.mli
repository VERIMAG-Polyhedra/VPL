open ASTerm
open BinInt
open CstrC
open LinTerm
open NumC
open Qcanon

module QAtomicCond :
 sig
  type atomicCond = { cmpOp : cmpG; right : QTerm.t }

  val cmpOp : atomicCond -> cmpG

  val right : atomicCond -> QTerm.t

  type t = atomicCond

  val make : QTerm.term -> cmpG -> QTerm.term -> t

  val toCstr : cmpT -> QAffTerm.t -> Cstr.t
 end

module ZAtomicCond :
 sig
  type atomicCond = { cmpOp : cmpG; right : ZTerm.t }

  val cmpOp : atomicCond -> cmpG

  val right : atomicCond -> ZTerm.t

  type t = atomicCond

  val make : ZTerm.term -> cmpG -> ZTerm.term -> t

  val toCstr : cmpT -> ZAffTerm.t -> Cstr.t
 end

module ZtoQCstr :
 sig
  type t = Cstr.t
 end
