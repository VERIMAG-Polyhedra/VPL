open BinNums
open BinPos
open Datatypes

module N :
 sig
  val mul : coq_N -> coq_N -> coq_N

  val of_nat : nat -> coq_N
 end
