open Debugging
open LinTerm
open NumC
open ProgVar
open Qcanon
open Ring_polynom
open Ring_polynom_AddOnQ
open String0

module Cstr :
 sig
  type tInd = { coefs : LinQ.t; typ : cmpT; cst : QNum.t }

  val coefs : tInd -> LinQ.t

  val typ : tInd -> cmpT

  val cst : tInd -> QNum.t

  type t = tInd

  val cmpPr : cmpT -> char list

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  val top : tInd

  val triv : cmpT -> QNum.t -> tInd

  val to_le : t -> t

  val isContrad : t -> bool

  val isEq : t -> t -> bool

  val cmpAdd : cmpT -> cmpT -> cmpT

  val add : t -> t -> t

  val mulSimpl : t -> QNum.t -> t

  val mul : QNum.t -> t -> t

  val merge : tInd -> tInd -> t

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> tInd -> t

  val upperToCstr : LinQ.t -> QNum.t -> t

  val upperOrEqualsToCstr : LinQ.t -> QNum.t -> t

  val lowerToCstr : LinQ.t -> QNum.t -> t

  val lowerOrEqualsToCstr : LinQ.t -> QNum.t -> t

  val to_PExpr : t -> coq_PExpr
 end
