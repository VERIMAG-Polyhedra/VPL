open QArith_base
open Qreduction
open Specif

type coq_Qc = coq_Q
  (* singleton inductive, whose constructor was Qcmake *)

(** val this : coq_Qc -> coq_Q **)

let this q =
  q

(** val coq_Q2Qc : coq_Q -> coq_Qc **)

let coq_Q2Qc =
  coq_Qred

(** val coq_Qc_eq_dec : coq_Qc -> coq_Qc -> bool **)

let coq_Qc_eq_dec x y =
  coq_Qeq_dec (this x) (this y)

(** val coq_Qcplus : coq_Qc -> coq_Qc -> coq_Qc **)

let coq_Qcplus x y =
  coq_Q2Qc (coq_Qplus (this x) (this y))

(** val coq_Qcmult : coq_Qc -> coq_Qc -> coq_Qc **)

let coq_Qcmult x y =
  coq_Q2Qc (coq_Qmult (this x) (this y))

(** val coq_Qcopp : coq_Qc -> coq_Qc **)

let coq_Qcopp x =
  coq_Q2Qc (coq_Qopp (this x))

(** val coq_Qcminus : coq_Qc -> coq_Qc -> coq_Qc **)

let coq_Qcminus x y =
  coq_Qcplus x (coq_Qcopp y)

(** val coq_Qcinv : coq_Qc -> coq_Qc **)

let coq_Qcinv x =
  coq_Q2Qc (coq_Qinv (this x))

(** val coq_Qcdiv : coq_Qc -> coq_Qc -> coq_Qc **)

let coq_Qcdiv x y =
  coq_Qcmult x (coq_Qcinv y)

(** val coq_Qc_dec : coq_Qc -> coq_Qc -> bool sumor **)

let coq_Qc_dec x y =
  coq_Q_dec (this x) (this y)

(** val coq_Qclt_le_dec : coq_Qc -> coq_Qc -> bool **)

let coq_Qclt_le_dec x y =
  coq_Qlt_le_dec (this x) (this y)
