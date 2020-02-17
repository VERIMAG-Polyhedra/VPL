open BinInt
open BinNums
open QArith_base

(** val coq_Qfloor : coq_Q -> coq_Z **)

let coq_Qfloor x =
  let { coq_Qnum = n; coq_Qden = d } = x in Z.div n (Zpos d)

(** val coq_Qceiling : coq_Q -> coq_Z **)

let coq_Qceiling x =
  Z.opp (coq_Qfloor (coq_Qopp x))
