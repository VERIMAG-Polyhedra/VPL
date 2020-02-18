open BinInt
open BinNums
open CoqAddOn
open QArith_base
open Qcanon
open Qround
open Specif
open ZArith_dec

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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

(** val cmpG2T : cmpG -> cmpT option **)

let cmpG2T = function
| Eq -> Some EqT
| Le -> Some LeT
| Lt -> Some LtT
| Neq -> None

(** val cmpT_eq : cmpT -> cmpT -> bool **)

let cmpT_eq cmp1 cmp2 =
  match cmp1 with
  | EqT -> (match cmp2 with
            | EqT -> true
            | _ -> false)
  | LeT -> (match cmp2 with
            | LeT -> true
            | _ -> false)
  | LtT -> (match cmp2 with
            | LtT -> true
            | _ -> false)

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

module QNum =
 struct
  type t = coq_Qc

  (** val z : t **)

  let z =
    coq_Q2Qc { coq_Qnum = Z0; coq_Qden = Coq_xH }

  (** val u : t **)

  let u =
    coq_Q2Qc { coq_Qnum = (Zpos Coq_xH); coq_Qden = Coq_xH }

  (** val add : t -> t -> t **)

  let add =
    coq_Qcplus

  (** val sub : t -> t -> t **)

  let sub =
    coq_Qcminus

  (** val mul : t -> t -> t **)

  let mul =
    coq_Qcmult

  (** val div : t -> t -> t **)

  let div =
    coq_Qcdiv

  (** val opp : t -> t **)

  let opp =
    coq_Qcopp

  (** val inv : t -> t **)

  let inv =
    coq_Qcinv

  (** val eqDec : t -> t -> bool **)

  let eqDec =
    coq_Qc_eq_dec

  (** val ltLeDec : t -> t -> bool **)

  let ltLeDec =
    coq_Qclt_le_dec

  (** val dec : t -> t -> bool sumor **)

  let dec =
    coq_Qc_dec

  (** val isZero : t -> bool **)

  let isZero n =
    let filtered_var = (this n).coq_Qnum in
    (match filtered_var with
     | Z0 -> true
     | _ -> false)

  (** val mulDiscr : t -> trivialMulDiscr **)

  let mulDiscr n =
    match (this n).coq_Qden with
    | Coq_xH ->
      (match (this n).coq_Qnum with
       | Z0 -> IsZero
       | Zpos p -> (match p with
                    | Coq_xH -> IsUnit
                    | _ -> Other)
       | Zneg p -> (match p with
                    | Coq_xH -> IsOppUnit
                    | _ -> Other))
    | _ -> Other

  (** val pr : t -> char list **)

  let pr q =
    let nstr = CoqPr.zPr' (this q).coq_Qnum in
    let dstr = CoqPr.posPr' (this q).coq_Qden in
    sprintf ('('::('%'::('s'::('/'::('%'::('s'::(')'::[])))))))
      (nstr::(dstr::[]))

  (** val prRaw : t -> char list **)

  let prRaw q =
    let nstr = CoqPr.zPrRaw' (this q).coq_Qnum in
    let dstr = CoqPr.posPrRaw' (this q).coq_Qden in
    sprintf
      ('{'::('n'::('u'::('m'::(' '::('='::(' '::('%'::('s'::(';'::(' '::('d'::('e'::('n'::(' '::('='::(' '::('%'::('s'::('}'::[]))))))))))))))))))))
      (nstr::(dstr::[]))

  (** val to_Q : t -> coq_Q **)

  let to_Q =
    this

  (** val cmpDenote_dec : cmpG -> t -> t -> bool **)

  let cmpDenote_dec cmp n1 n2 =
    match cmp with
    | Eq -> coq_Qc_eq_dec n1 n2
    | Le -> if coq_Qclt_le_dec n2 n1 then false else true
    | Lt -> coq_Qclt_le_dec n1 n2
    | Neq -> if coq_Qc_eq_dec n1 n2 then false else true
 end

module ZNum =
 struct
  type t = coq_Z

  (** val z : t **)

  let z =
    Z0

  (** val u : t **)

  let u =
    Zpos Coq_xH

  (** val eqDec : t -> t -> bool **)

  let eqDec =
    Z.eq_dec

  (** val ltLeDec : t -> t -> bool **)

  let ltLeDec =
    coq_Z_lt_le_dec

  (** val dec : t -> t -> bool sumor **)

  let dec =
    coq_Z_dec'

  (** val add : t -> t -> t **)

  let add =
    Z.add

  (** val mul : t -> t -> t **)

  let mul =
    Z.mul

  (** val opp : t -> t **)

  let opp =
    Z.opp

  (** val pr : t -> char list **)

  let pr =
    CoqPr.zPr'

  (** val prRaw : t -> char list **)

  let prRaw =
    CoqPr.zPrRaw'

  (** val sub : t -> t -> t **)

  let sub =
    Z.sub

  (** val isZero : t -> bool **)

  let isZero = function
  | Z0 -> true
  | _ -> false

  (** val mulDiscr : t -> trivialMulDiscr **)

  let mulDiscr = function
  | Z0 -> IsZero
  | Zpos p -> (match p with
               | Coq_xH -> IsUnit
               | _ -> Other)
  | Zneg p -> (match p with
               | Coq_xH -> IsOppUnit
               | _ -> Other)

  (** val cmpDenote_dec : cmpG -> t -> t -> bool **)

  let cmpDenote_dec cmp n1 n2 =
    match cmp with
    | Eq -> Z.eq_dec n1 n2
    | Le -> if coq_Z_lt_le_dec n2 n1 then false else true
    | Lt -> coq_Z_lt_le_dec n1 n2
    | Neq -> if Z.eq_dec n1 n2 then false else true
 end

module ZtoQ =
 struct
  (** val ofZ : ZNum.t -> QNum.t **)

  let ofZ =
    inject_Z

  (** val isInZ : QNum.t -> __ option **)

  let isInZ q =
    let filtered_var = (this q).coq_Qden in
    (match filtered_var with
     | Coq_xH -> Some __
     | _ -> None)

  (** val floor : QNum.t -> ZNum.t **)

  let floor n =
    coq_Qfloor (this n)

  (** val ceil : QNum.t -> ZNum.t **)

  let ceil n =
    coq_Qceiling (this n)
 end
