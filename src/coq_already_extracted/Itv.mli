open BinInt
open BinNums
open NumC
open Qcanon
open Specif
open String0

module QItv :
 sig
  type bndT =
  | Infty
  | Open of QNum.t
  | Closed of QNum.t

  val bndT_rect : 'a1 -> (QNum.t -> 'a1) -> (QNum.t -> 'a1) -> bndT -> 'a1

  val bndT_rec : 'a1 -> (QNum.t -> 'a1) -> (QNum.t -> 'a1) -> bndT -> 'a1

  type tInd = { lower : bndT; upper : bndT }

  val lower : tInd -> bndT

  val upper : tInd -> bndT

  type t = tInd

  val top : t

  val bot : t

  val is_not_upper : QNum.t -> bndT -> bool

  val is_bot : t -> bool

  val mk1 : QNum.t -> t

  val bndAdd : bndT -> bndT -> bndT

  val add : tInd -> tInd -> tInd

  val bndOpp : bndT -> bndT

  val opp : tInd -> tInd

  val bndMulc : bndT -> QNum.t -> bndT

  val mulcPos : t -> QNum.t -> tInd

  val mulc : t -> QNum.t -> tInd

  val pr : t -> char list

  val shift : t -> QNum.t -> tInd
 end

module ZItv :
 sig
  type bndT =
  | Infty
  | Open of ZNum.t
  | Closed of ZNum.t

  type tInd = { lower : bndT; upper : bndT }

  val lower : tInd -> bndT

  val upper : tInd -> bndT

  type t = tInd

  val tightenL : QItv.bndT -> bndT

  val tightenU : QItv.bndT -> bndT

  val fromQItv : QItv.t -> t
 end
