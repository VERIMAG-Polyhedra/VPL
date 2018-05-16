open NumC
open Qcanon
open Itv
open DomainInterfaces

module QN = struct
  type t = coq_Qc option

  let add qn1 qn2 =
    match qn1, qn2 with
    | Some q1, Some q2 -> Some (QNum.add q1 q2)
    | _,_ -> None

  (** val opp : t -> t **)
  let opp = function
  | Some q -> Some (QNum.opp q)
  | None -> None

  let max : coq_Qc -> coq_Qc -> coq_Qc
    = fun q1 q2 ->
    if coq_Qclt_le_dec q1 q2
    then q2
    else q1

let min : coq_Qc -> coq_Qc -> coq_Qc
  = fun q1 q2 ->
  if coq_Qclt_le_dec q1 q2
  then q1
  else q2

  (** val join : t -> t -> t **)

  let join qn1 qn2 =
    match qn1, qn2 with
    | Some q1, Some q2 -> Some (max q1 q2)
    | _,_ -> None

  let mul qn1 qn2 =
    match qn1, qn2 with
    | Some q1, Some q2 -> Some (QNum.mul q1 q2)
    | _,_ -> None

  (** val meet : t -> t -> t **)

  let meet qn1 qn2 =
    match qn1, qn2 with
    | Some q1, Some q2 -> Some (min q1 q2)
    | _,_ -> None
 end

module QNItv = struct

  type itv = { low : QN.t; up : QN.t }

  (** val low : itv -> QN.t **)

  let low x = x.low

  (** val up : itv -> QN.t **)

  let up x = x.up

  type t = itv

  (** val top : itv **)

  let top =
    { low = None; up = None }

  let fromBndT = function
  | QItv.Infty -> None
  | QItv.Open n | QItv.Closed n -> Some n

  let oppMode = function
  | BOTH -> BOTH
  | UP -> LOW
  | LOW -> UP

  let fromQItv i =
    { low = (fromBndT i.QItv.lower); up = (fromBndT i.QItv.upper) }

    (** val single : coq_Z -> itv **)

  let single z =
    let zn = Some z in { low = zn; up = zn }

  (** val add : mode -> itv -> itv -> itv **)

  let add m i1 i2 =
    match m with
    | BOTH -> { low = QN.add i1.low i2.low; up = QN.add i1.up i2.up }
    | UP -> { low = None; up = QN.add i1.up i2.up }
    | LOW -> { low = QN.add i1.low i2.low; up = None}

  (** val opp : itv -> itv **)

  let opp i = { low = QN.opp i.low ; up = QN.opp i.up }

  (* mul (l1,u1) (l2,u2) = min (l1*l2, l1*u12, u1 * l2, u1 * u2), ... *)
let mul m i1 i2 =
    match m with
    | BOTH -> let max = QN.join
        (QN.join
            (QN.join
                (QN.mul i1.low i2.low)
                (QN.mul i1.low i2.up))
            (QN.mul i1.up i2.low))
        (QN.mul i1.up i2.up)
        in
        let min = QN.meet
            (QN.meet
                (QN.meet
                    (QN.mul i1.low i2.low)
                    (QN.mul i1.low i2.up))
                (QN.mul i1.up i2.low))
            (QN.mul i1.up i2.up)
        in
        { low = min ; up = max }
    | UP -> let max = QN.join
        (QN.join
            (QN.join
                (QN.mul i1.low i2.low)
                (QN.mul i1.low i2.up))
            (QN.mul i1.up i2.low))
        (QN.mul i1.up i2.up)
        in
        { low = None; up = max }
    | LOW -> let min = QN.meet
        (QN.meet
            (QN.meet
                (QN.mul i1.low i2.low)
                (QN.mul i1.low i2.up))
            (QN.mul i1.up i2.low))
        (QN.mul i1.up i2.up)
    in
    { low = min ; up = None }

  (** val join : mode -> itv -> itv -> itv **)

  let join m i1 i2 =
    match m with
    | BOTH -> { low = (QN.meet i1.low i2.low); up = (QN.join i1.up i2.up) }
    | UP -> { low = None; up = (QN.join i1.up i2.up) }
    | LOW -> { low = (QN.meet i1.low i2.low); up = None }

 end
