open BinInt
open BinNums

val coq_Z_isZero : coq_Z -> bool

val coq_Z_isNat : coq_Z -> bool

val coq_Z_isNegNat : coq_Z -> bool

module ZN :
 sig
  type t = coq_Z option

  val add : t -> t -> t

  val mulZ1 : coq_Z -> t -> t

  val mulZ : coq_Z -> t -> t

  val opp : t -> t

  val isZero : coq_Z option -> bool

  val join : t -> t -> t

  val meet : t -> t -> t
 end
