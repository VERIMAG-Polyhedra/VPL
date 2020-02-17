open BinInt
open BinNums
open Datatypes
open Specif

type __ = Obj.t

val coq_Zcompare_rect :
  coq_Z -> coq_Z -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1

val coq_Zcompare_rec :
  coq_Z -> coq_Z -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1

val coq_Z_lt_dec : coq_Z -> coq_Z -> bool

val coq_Z_lt_ge_dec : coq_Z -> coq_Z -> bool

val coq_Z_lt_le_dec : coq_Z -> coq_Z -> bool

val coq_Z_le_lt_eq_dec : coq_Z -> coq_Z -> bool

val not_Zeq_inf : coq_Z -> coq_Z -> bool

val coq_Z_dec' : coq_Z -> coq_Z -> bool sumor
