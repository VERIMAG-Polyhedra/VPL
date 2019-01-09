open BinInt
open BinNums
open NumC
open Qcanon
open Specif
open String0

module QItv =
 struct
  type bndT =
  | Infty
  | Open of QNum.t
  | Closed of QNum.t

  (** val bndT_rect :
      'a1 -> (QNum.t -> 'a1) -> (QNum.t -> 'a1) -> bndT -> 'a1 **)

  let bndT_rect f f0 f1 = function
  | Infty -> f
  | Open x -> f0 x
  | Closed x -> f1 x

  (** val bndT_rec :
      'a1 -> (QNum.t -> 'a1) -> (QNum.t -> 'a1) -> bndT -> 'a1 **)

  let bndT_rec f f0 f1 = function
  | Infty -> f
  | Open x -> f0 x
  | Closed x -> f1 x

  type tInd = { lower : bndT; upper : bndT }

  (** val lower : tInd -> bndT **)

  let lower x = x.lower

  (** val upper : tInd -> bndT **)

  let upper x = x.upper

  type t = tInd

  (** val top : t **)

  let top =
    { lower = Infty; upper = Infty }

  (** val bot : t **)

  let bot =
    { lower = (Closed QNum.u); upper = (Closed QNum.z) }

  (** val is_not_upper : QNum.t -> bndT -> bool **)

  let is_not_upper n = function
  | Infty -> false
  | Open m -> if coq_Qclt_le_dec n m then false else true
  | Closed m -> if coq_Qclt_le_dec m n then true else false

  (** val is_bot : t -> bool **)

  let is_bot i =
    match i.lower with
    | Infty -> false
    | Open n ->
      (match i.upper with
       | Infty -> false
       | Open m -> if coq_Qclt_le_dec n m then false else true
       | Closed m -> if coq_Qclt_le_dec n m then false else true)
    | Closed n -> is_not_upper n i.upper

  (** val mk1 : QNum.t -> t **)

  let mk1 n =
    { lower = (Closed n); upper = (Closed n) }

  (** val bndAdd : bndT -> bndT -> bndT **)

  let bndAdd b1 b2 =
    match b1 with
    | Infty -> Infty
    | Open n1 ->
      (match b2 with
       | Infty -> Infty
       | Open n2 -> Open (coq_Qcplus n1 n2)
       | Closed n2 -> Open (coq_Qcplus n1 n2))
    | Closed n1 ->
      (match b2 with
       | Infty -> Infty
       | Open n2 -> Open (coq_Qcplus n1 n2)
       | Closed n2 -> Closed (coq_Qcplus n1 n2))

  (** val add : tInd -> tInd -> tInd **)

  let add i1 i2 =
    { lower = (bndAdd i1.lower i2.lower); upper = (bndAdd i1.upper i2.upper) }

  (** val bndOpp : bndT -> bndT **)

  let bndOpp = function
  | Infty -> Infty
  | Open n -> Open (coq_Qcopp n)
  | Closed n -> Closed (coq_Qcopp n)

  (** val opp : tInd -> tInd **)

  let opp i =
    { lower = (bndOpp i.upper); upper = (bndOpp i.lower) }

  (** val bndMulc : bndT -> QNum.t -> bndT **)

  let bndMulc i n =
    match i with
    | Infty -> Infty
    | Open n' -> Open (coq_Qcmult n n')
    | Closed n' -> Closed (coq_Qcmult n n')

  (** val mulcPos : t -> QNum.t -> tInd **)

  let mulcPos i n =
    { lower = (bndMulc i.lower n); upper = (bndMulc i.upper n) }

  (** val mulc : t -> QNum.t -> tInd **)

  let mulc i n =
    match coq_Qc_dec QNum.z n with
    | Coq_inleft pfn ->
      if pfn then mulcPos i n else opp (mulcPos i (coq_Qcopp n))
    | Coq_inright -> mk1 QNum.z

  (** val pr : t -> char list **)

  let pr i =
    let lstr =
      match i.lower with
      | Infty -> ']'::('-'::('i'::('n'::('f'::('t'::('y'::[]))))))
      | Open n -> append (']'::[]) (QNum.pr n)
      | Closed n -> append ('['::[]) (QNum.pr n)
    in
    let ustr =
      match i.upper with
      | Infty -> '+'::('i'::('n'::('f'::('t'::('y'::('['::[]))))))
      | Open n -> append (QNum.pr n) ('['::[])
      | Closed n -> append (QNum.pr n) (']'::[])
    in
    append lstr (append (','::(' '::[])) ustr)

  (** val shift : t -> QNum.t -> tInd **)

  let shift i n =
    add (mk1 n) i
 end

module ZItv =
 struct
  type bndT =
  | Infty
  | Open of ZNum.t
  | Closed of ZNum.t

  type tInd = { lower : bndT; upper : bndT }

  (** val lower : tInd -> bndT **)

  let lower x = x.lower

  (** val upper : tInd -> bndT **)

  let upper x = x.upper

  type t = tInd

  (** val tightenL : QItv.bndT -> bndT **)

  let tightenL = function
  | QItv.Infty -> Infty
  | QItv.Open q ->
    (match ZtoQ.isInZ q with
     | Some _ -> Closed (Z.add (ZtoQ.ceil q) (Zpos Coq_xH))
     | None -> Closed (ZtoQ.ceil q))
  | QItv.Closed q -> Closed (ZtoQ.ceil q)

  (** val tightenU : QItv.bndT -> bndT **)

  let tightenU = function
  | QItv.Infty -> Infty
  | QItv.Open q ->
    (match ZtoQ.isInZ q with
     | Some _ -> Closed (Z.sub (ZtoQ.floor q) (Zpos Coq_xH))
     | None -> Closed (ZtoQ.floor q))
  | QItv.Closed q -> Closed (ZtoQ.floor q)

  (** val fromQItv : QItv.t -> t **)

  let fromQItv qi =
    { lower = (tightenL qi.QItv.lower); upper = (tightenU qi.QItv.upper) }
 end
