open QArith_base
open Qreduction
open Specif

type coq_Qc = coq_Q
  (* singleton inductive, whose constructor was Qcmake *)

val this : coq_Qc -> coq_Q

val coq_Q2Qc : coq_Q -> coq_Qc

val coq_Qc_eq_dec : coq_Qc -> coq_Qc -> bool

val coq_Qcplus : coq_Qc -> coq_Qc -> coq_Qc

val coq_Qcmult : coq_Qc -> coq_Qc -> coq_Qc

val coq_Qcopp : coq_Qc -> coq_Qc

val coq_Qcminus : coq_Qc -> coq_Qc -> coq_Qc

val coq_Qcinv : coq_Qc -> coq_Qc

val coq_Qcdiv : coq_Qc -> coq_Qc -> coq_Qc

val coq_Qc_dec : coq_Qc -> coq_Qc -> bool sumor

val coq_Qclt_le_dec : coq_Qc -> coq_Qc -> bool
