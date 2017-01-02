open BinNums
open BinPos
open Datatypes

module Z :
 sig
  val double : coq_Z -> coq_Z

  val succ_double : coq_Z -> coq_Z

  val pred_double : coq_Z -> coq_Z

  val pos_sub : positive -> positive -> coq_Z

  val add : coq_Z -> coq_Z -> coq_Z

  val opp : coq_Z -> coq_Z

  val sub : coq_Z -> coq_Z -> coq_Z

  val mul : coq_Z -> coq_Z -> coq_Z

  val compare : coq_Z -> coq_Z -> comparison

  val sgn : coq_Z -> coq_Z

  val leb : coq_Z -> coq_Z -> bool

  val ltb : coq_Z -> coq_Z -> bool

  val eqb : coq_Z -> coq_Z -> bool

  val max : coq_Z -> coq_Z -> coq_Z

  val min : coq_Z -> coq_Z -> coq_Z

  val abs : coq_Z -> coq_Z

  val to_pos : coq_Z -> positive

  val pos_div_eucl : positive -> coq_Z -> coq_Z*coq_Z

  val div_eucl : coq_Z -> coq_Z -> coq_Z*coq_Z

  val div : coq_Z -> coq_Z -> coq_Z

  val ggcd : coq_Z -> coq_Z -> coq_Z*(coq_Z*coq_Z)

  val eq_dec : coq_Z -> coq_Z -> bool
 end
