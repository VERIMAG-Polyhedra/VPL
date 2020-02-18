open BinNums
open BinPosDef
open Datatypes
open Nat0

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  val succ_double_mask : mask -> mask

  val double_mask : mask -> mask

  val double_pred_mask : positive -> mask

  val sub_mask : positive -> positive -> mask

  val sub_mask_carry : positive -> positive -> mask

  val sub : positive -> positive -> positive

  val mul : positive -> positive -> positive

  val size_nat : positive -> nat

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val max : positive -> positive -> positive

  val eqb : positive -> positive -> bool

  val leb : positive -> positive -> bool

  val ltb : positive -> positive -> bool

  val ggcdn : nat -> positive -> positive -> positive*(positive*positive)

  val ggcd : positive -> positive -> positive*(positive*positive)

  val of_succ_nat : nat -> positive

  val eq_dec : positive -> positive -> bool
 end
