open BinNums
open BinPos

module PVar :
 sig
  type t = positive

  val isEq : t -> t -> bool

  val eq_dec : t -> t -> bool

  val isLt : t -> t -> bool

  val isLe : t -> t -> bool

  val export : t -> positive

  val import : positive -> t

  val max : t -> t -> t

  val pr : t -> char list
 end

module Mem :
 sig
  type 'a t = PVar.t -> 'a

  val assign : PVar.t -> 'a1 -> 'a1 t -> PVar.t -> 'a1

  val lift : ('a1 -> 'a2) -> 'a1 t -> 'a2 t
 end
