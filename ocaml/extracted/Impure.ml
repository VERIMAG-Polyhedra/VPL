
module type ImpureMonad =
 sig
  type 'x imp

  val pure : 'a1 -> 'a1 imp

  val bind : 'a1 imp -> ('a1 -> 'a2 imp) -> 'a2 imp
 end

module type FullImpureMonad =
 sig
  module Base :
   ImpureMonad
 end
