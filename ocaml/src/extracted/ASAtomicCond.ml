open ASTerm
open BinInt
open CstrC
open LinTerm
open NumC
open Qcanon

module QAtomicCond =
 struct
  type atomicCond = { cmpOp : cmpG; right : QTerm.t }

  (** val cmpOp : atomicCond -> cmpG **)

  let cmpOp x = x.cmpOp

  (** val right : atomicCond -> QTerm.t **)

  let right x = x.right

  type t = atomicCond

  (** val make : QTerm.term -> cmpG -> QTerm.term -> t **)

  let make t1 cmp t2 =
    { cmpOp = cmp; right =
      (QTerm.smartAdd t2
        (match t1 with
         | QTerm.Cte c -> QTerm.Cte (coq_Qcopp c)
         | _ -> QTerm.Opp t1)) }

  (** val toCstr : cmpT -> QAffTerm.t -> Cstr.t **)

  let toCstr cmp aft =
    { Cstr.coefs = (LinQ.opp aft.QAffTerm.lin); Cstr.typ = cmp; Cstr.cst =
      aft.QAffTerm.cte }
 end

module ZAtomicCond =
 struct
  type atomicCond = { cmpOp : cmpG; right : ZTerm.t }

  (** val cmpOp : atomicCond -> cmpG **)

  let cmpOp x = x.cmpOp

  (** val right : atomicCond -> ZTerm.t **)

  let right x = x.right

  type t = atomicCond

  (** val make : ZTerm.term -> cmpG -> ZTerm.term -> t **)

  let make t1 cmp t2 =
    { cmpOp = cmp; right =
      (ZTerm.smartAdd t2
        (match t1 with
         | ZTerm.Cte c -> ZTerm.Cte (Z.opp c)
         | _ -> ZTerm.Opp t1)) }

  (** val toCstr : cmpT -> ZAffTerm.t -> Cstr.t **)

  let toCstr cmp aft =
    QAtomicCond.toCstr cmp (QAffTerm.lift aft)
 end

module ZtoQCstr =
 struct
  type t = Cstr.t
 end
