open BinInt
open BinNums
open Datatypes
open FMapPositive
open List0
open NumC
open PositiveMapAddOn
open ProgVar
open QArith_base
open Qcanon
open Ring_polynom
open Ring_polynom_AddOnQ
open String0

module type LinSig =
 functor (N:NumSig) ->
 sig
  type t

  val eval : t -> N.t Mem.t -> N.t

  val nil : t

  val isNil : t -> bool

  val single : PVar.t -> N.t -> t

  val opp : t -> t

  val mul : N.t -> t -> t

  val isEq : t -> t -> bool

  val add : t -> t -> t

  type exportT = (PVar.t*N.t) list

  val export : t -> exportT

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list
 end

module PositiveMapVec :
 functor (N:NumSig) ->
 sig
  type t = N.t PositiveMap.t

  val absEval : (PVar.t*N.t) list -> N.t Mem.t -> N.t

  val eval : t -> N.t Mem.t -> N.t

  type exportT = (PVar.t*N.t) list

  val export : t -> exportT

  val nil : t

  val isNil : t -> bool

  val single : PVar.t -> N.t -> t

  val opp : t -> N.t PositiveMap.t

  val mul : N.t -> t -> t

  val coq_N_eqb : N.t -> N.t -> bool

  val isEq : t -> t -> bool

  val add : t -> t -> t

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  val fmtAux : char list list -> char list -> char list

  val fmt : char list list -> char list

  val pairPr : (PVar.t*N.t) -> char list

  val pr : t -> char list

  val pair_to_string : (PVar.t -> char list) -> (PVar.t*N.t) -> char list

  val to_string : (PVar.t -> char list) -> t -> char list
 end

module LinZ :
 sig
  type t = ZNum.t PositiveMap.t

  val absEval : (PVar.t*ZNum.t) list -> ZNum.t Mem.t -> ZNum.t

  val eval : t -> ZNum.t Mem.t -> ZNum.t

  type exportT = (PVar.t*ZNum.t) list

  val export : t -> exportT

  val nil : t

  val isNil : t -> bool

  val single : PVar.t -> ZNum.t -> t

  val opp : t -> ZNum.t PositiveMap.t

  val mul : ZNum.t -> t -> t

  val coq_N_eqb : ZNum.t -> ZNum.t -> bool

  val isEq : t -> t -> bool

  val add : t -> t -> t

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  val fmtAux : char list list -> char list -> char list

  val fmt : char list list -> char list

  val pairPr : (PVar.t*ZNum.t) -> char list

  val pr : t -> char list

  val pair_to_string : (PVar.t -> char list) -> (PVar.t*ZNum.t) -> char list

  val to_string : (PVar.t -> char list) -> t -> char list
 end

module LinQ :
 sig
  type t = QNum.t PositiveMap.t

  val absEval : (PVar.t*QNum.t) list -> QNum.t Mem.t -> QNum.t

  val eval : t -> QNum.t Mem.t -> QNum.t

  type exportT = (PVar.t*QNum.t) list

  val export : t -> exportT

  val nil : t

  val isNil : t -> bool

  val single : PVar.t -> QNum.t -> t

  val opp : t -> QNum.t PositiveMap.t

  val mul : QNum.t -> t -> t

  val coq_N_eqb : QNum.t -> QNum.t -> bool

  val isEq : t -> t -> bool

  val add : t -> t -> t

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  val fmtAux : char list list -> char list -> char list

  val fmt : char list list -> char list

  val pairPr : (PVar.t*QNum.t) -> char list

  val pr : t -> char list

  val pair_to_string : (PVar.t -> char list) -> (PVar.t*QNum.t) -> char list

  val to_string : (PVar.t -> char list) -> t -> char list

  val exportT_to_PExpr : exportT -> coq_PExpr

  val to_PExpr : t -> coq_PExpr

  val mem_compat : QNum.t Mem.t -> positive -> coq_Q

  val import : exportT -> t

  val lift : LinZ.t -> t
 end

module AffineTerm :
 functor (N:NumSig) ->
 functor (L:sig
  type t

  val eval : t -> N.t Mem.t -> N.t

  val nil : t

  val isNil : t -> bool

  val single : PVar.t -> N.t -> t

  val opp : t -> t

  val mul : N.t -> t -> t

  val isEq : t -> t -> bool

  val add : t -> t -> t

  type exportT = (PVar.t*N.t) list

  val export : t -> exportT

  val isFree : PVar.t -> t -> bool

  val rename : PVar.t -> PVar.t -> t -> t

  val pr : t -> char list

  val to_string : (PVar.t -> char list) -> t -> char list
 end) ->
 sig
  type affTerm = { lin : L.t; cte : N.t }

  val lin : affTerm -> L.t

  val cte : affTerm -> N.t

  type t = affTerm

  val eval : affTerm -> N.t Mem.t -> N.t

  val nil : affTerm

  val opp : affTerm -> affTerm

  val mul : N.t -> affTerm -> affTerm

  val add : affTerm -> affTerm -> affTerm

  val addc : N.t -> affTerm -> affTerm

  val addx : PVar.t -> affTerm -> affTerm

  val addnx : PVar.t -> affTerm -> affTerm

  val isZero : affTerm -> bool
 end

module type AffineTermSig =
 functor (N:NumSig) ->
 sig
  module Lin :
   sig
    type t

    val eval : t -> N.t Mem.t -> N.t

    val nil : t

    val isNil : t -> bool

    val single : PVar.t -> N.t -> t

    val opp : t -> t

    val mul : N.t -> t -> t

    val isEq : t -> t -> bool

    val add : t -> t -> t

    type exportT = (PVar.t*N.t) list

    val export : t -> exportT

    val isFree : PVar.t -> t -> bool

    val rename : PVar.t -> PVar.t -> t -> t

    val pr : t -> char list

    val to_string : (PVar.t -> char list) -> t -> char list
   end

  type affTerm = { lin : Lin.t; cte : N.t }

  val lin : affTerm -> Lin.t

  val cte : affTerm -> N.t

  type t = affTerm

  val eval : affTerm -> N.t Mem.t -> N.t

  val nil : affTerm

  val opp : affTerm -> affTerm

  val mul : N.t -> affTerm -> affTerm

  val add : affTerm -> affTerm -> affTerm

  val addc : N.t -> affTerm -> affTerm

  val addx : PVar.t -> affTerm -> affTerm

  val addnx : PVar.t -> affTerm -> affTerm

  val isZero : affTerm -> bool
 end

module ZAffTerm :
 sig
  module Lin :
   sig
    type t = ZNum.t PositiveMap.t

    val absEval : (PVar.t*ZNum.t) list -> ZNum.t Mem.t -> ZNum.t

    val eval : t -> ZNum.t Mem.t -> ZNum.t

    type exportT = (PVar.t*ZNum.t) list

    val export : t -> exportT

    val nil : t

    val isNil : t -> bool

    val single : PVar.t -> ZNum.t -> t

    val opp : t -> ZNum.t PositiveMap.t

    val mul : ZNum.t -> t -> t

    val coq_N_eqb : ZNum.t -> ZNum.t -> bool

    val isEq : t -> t -> bool

    val add : t -> t -> t

    val isFree : PVar.t -> t -> bool

    val rename : PVar.t -> PVar.t -> t -> t

    val fmtAux : char list list -> char list -> char list

    val fmt : char list list -> char list

    val pairPr : (PVar.t*ZNum.t) -> char list

    val pr : t -> char list

    val pair_to_string : (PVar.t -> char list) -> (PVar.t*ZNum.t) -> char list

    val to_string : (PVar.t -> char list) -> t -> char list
   end

  type affTerm = { lin : LinZ.t; cte : ZNum.t }

  val lin : affTerm -> LinZ.t

  val cte : affTerm -> ZNum.t

  type t = affTerm

  val eval : affTerm -> ZNum.t Mem.t -> ZNum.t

  val nil : affTerm

  val opp : affTerm -> affTerm

  val mul : ZNum.t -> affTerm -> affTerm

  val add : affTerm -> affTerm -> affTerm

  val addc : ZNum.t -> affTerm -> affTerm

  val addx : PVar.t -> affTerm -> affTerm

  val addnx : PVar.t -> affTerm -> affTerm

  val isZero : affTerm -> bool
 end

module QAffTerm :
 sig
  module Lin :
   sig
    type t = QNum.t PositiveMap.t

    val absEval : (PVar.t*QNum.t) list -> QNum.t Mem.t -> QNum.t

    val eval : t -> QNum.t Mem.t -> QNum.t

    type exportT = (PVar.t*QNum.t) list

    val export : t -> exportT

    val nil : t

    val isNil : t -> bool

    val single : PVar.t -> QNum.t -> t

    val opp : t -> QNum.t PositiveMap.t

    val mul : QNum.t -> t -> t

    val coq_N_eqb : QNum.t -> QNum.t -> bool

    val isEq : t -> t -> bool

    val add : t -> t -> t

    val isFree : PVar.t -> t -> bool

    val rename : PVar.t -> PVar.t -> t -> t

    val fmtAux : char list list -> char list -> char list

    val fmt : char list list -> char list

    val pairPr : (PVar.t*QNum.t) -> char list

    val pr : t -> char list

    val pair_to_string : (PVar.t -> char list) -> (PVar.t*QNum.t) -> char list

    val to_string : (PVar.t -> char list) -> t -> char list

    val exportT_to_PExpr : exportT -> coq_PExpr

    val to_PExpr : t -> coq_PExpr

    val mem_compat : QNum.t Mem.t -> positive -> coq_Q

    val import : exportT -> t

    val lift : LinZ.t -> t
   end

  type affTerm = { lin : LinQ.t; cte : QNum.t }

  val lin : affTerm -> LinQ.t

  val cte : affTerm -> QNum.t

  type t = affTerm

  val eval : affTerm -> QNum.t Mem.t -> QNum.t

  val nil : affTerm

  val opp : affTerm -> affTerm

  val mul : QNum.t -> affTerm -> affTerm

  val add : affTerm -> affTerm -> affTerm

  val addc : QNum.t -> affTerm -> affTerm

  val addx : PVar.t -> affTerm -> affTerm

  val addnx : PVar.t -> affTerm -> affTerm

  val isZero : affTerm -> bool

  val lift : ZAffTerm.t -> t
 end
