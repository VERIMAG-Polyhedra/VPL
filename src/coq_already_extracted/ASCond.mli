open ASTerm
open BinInt
open BinNums
open Datatypes
open LinTerm
open NumC
open ProgVar
open Qcanon
open ZArith_dec

type binl =
| AND
| OR

module QCond :
 sig
  module Term :
   sig
    module Annot :
     sig
      type topLevelAnnot = TopLevelAnnot.topLevelAnnot =
      | OLD
      | AFFINE
      | INTERV
      | STATIC
      | SKIP_ORACLE

      val topLevelAnnot_rect :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      val topLevelAnnot_rec :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      type t = topLevelAnnot

      val pr : topLevelAnnot -> char list
     end

    type term = BasicQTerm.term =
    | Var of PVar.t
    | Cte of QNum.t
    | Add of term * term
    | Opp of term
    | Mul of term * term
    | Annot of Annot.topLevelAnnot * term

    val term_rect :
      (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    val term_rec :
      (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    type t = term

    val eval : term -> QNum.t Mem.t -> QNum.t

    val mdBound : term -> PVar.t -> PVar.t

    val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1

    val map : term -> PVar.t Mem.t -> term

    val pseudoIsZero : term -> bool

    val smartScalAdd1 : QNum.t -> term -> term

    val smartScalAdd : QNum.t -> term -> term

    val smartAdd : term -> term -> term

    val smartOpp : term -> term

    val smartScalMul1 : QNum.t -> term -> term

    val smartScalMul : QNum.t -> term -> term

    val smartMul : term -> term -> term

    val smartAnnot : Annot.topLevelAnnot -> term -> term

    val import_acc : (PVar.t*QNum.t) list -> term -> term

    val import : (PVar.t*QNum.t) list -> term

    val coq_Old : term -> term

    val xeval : term -> QNum.t Mem.t -> QNum.t Mem.t -> QNum.t

    val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term

    val isCte : term -> bool

    val annotAFFINEx : term -> term

    val annotAFFINE_rec : term -> term option

    val annotAFFINE : term -> term

    val matchCte : term -> QNum.t option

    val pr : term -> char list

    val fromLin : QAffTerm.Lin.t -> BasicQTerm.term

    val fromAff : QAffTerm.affTerm -> BasicQTerm.term

    val affineDecompose : BasicQTerm.term -> BasicQTerm.term*QAffTerm.t
   end

  type cond =
  | Basic of bool
  | Atom of cmpG * QTerm.term * QTerm.term
  | BinL of binl * cond * cond
  | Not of cond

  val cond_rect :
    (bool -> 'a1) -> (cmpG -> QTerm.term -> QTerm.term -> 'a1) -> (binl ->
    cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  val cond_rec :
    (bool -> 'a1) -> (cmpG -> QTerm.term -> QTerm.term -> 'a1) -> (binl ->
    cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  type t = cond

  val sat_dec : t -> QNum.t Mem.t -> bool

  val xsat_dec : t -> QNum.t Mem.t -> QNum.t Mem.t -> bool

  val mdBound : cond -> positive -> positive

  val map : cond -> PVar.t Mem.t -> cond

  val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond

  val dual : cmpG -> QTerm.term -> QTerm.term -> t

  val nnf : t -> t

  val nnfNot : t -> t
 end

module ZCond :
 sig
  module Term :
   sig
    module Annot :
     sig
      type topLevelAnnot = TopLevelAnnot.topLevelAnnot =
      | OLD
      | AFFINE
      | INTERV
      | STATIC
      | SKIP_ORACLE

      val topLevelAnnot_rect :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      val topLevelAnnot_rec :
        'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1

      type t = topLevelAnnot

      val pr : topLevelAnnot -> char list
     end

    type term = BasicZTerm.term =
    | Var of PVar.t
    | Cte of ZNum.t
    | Add of term * term
    | Opp of term
    | Mul of term * term
    | Annot of Annot.topLevelAnnot * term

    val term_rect :
      (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    val term_rec :
      (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    type t = term

    val eval : term -> ZNum.t Mem.t -> ZNum.t

    val mdBound : term -> PVar.t -> PVar.t

    val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1

    val map : term -> PVar.t Mem.t -> term

    val pseudoIsZero : term -> bool

    val smartScalAdd1 : ZNum.t -> term -> term

    val smartScalAdd : ZNum.t -> term -> term

    val smartAdd : term -> term -> term

    val smartOpp : term -> term

    val smartScalMul1 : ZNum.t -> term -> term

    val smartScalMul : ZNum.t -> term -> term

    val smartMul : term -> term -> term

    val smartAnnot : Annot.topLevelAnnot -> term -> term

    val import_acc : (PVar.t*ZNum.t) list -> term -> term

    val import : (PVar.t*ZNum.t) list -> term

    val coq_Old : term -> term

    val xeval : term -> ZNum.t Mem.t -> ZNum.t Mem.t -> ZNum.t

    val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term

    val isCte : term -> bool

    val annotAFFINEx : term -> term

    val annotAFFINE_rec : term -> term option

    val annotAFFINE : term -> term

    val matchCte : term -> ZNum.t option

    val pr : term -> char list

    val fromLin : ZAffTerm.Lin.t -> BasicZTerm.term

    val fromAff : ZAffTerm.affTerm -> BasicZTerm.term

    val affineDecompose : BasicZTerm.term -> BasicZTerm.term*ZAffTerm.t
   end

  type cond =
  | Basic of bool
  | Atom of cmpG * ZTerm.term * ZTerm.term
  | BinL of binl * cond * cond
  | Not of cond

  val cond_rect :
    (bool -> 'a1) -> (cmpG -> ZTerm.term -> ZTerm.term -> 'a1) -> (binl ->
    cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  val cond_rec :
    (bool -> 'a1) -> (cmpG -> ZTerm.term -> ZTerm.term -> 'a1) -> (binl ->
    cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  type t = cond

  val sat_dec : t -> ZNum.t Mem.t -> bool

  val xsat_dec : t -> ZNum.t Mem.t -> ZNum.t Mem.t -> bool

  val mdBound : cond -> positive -> positive

  val map : cond -> PVar.t Mem.t -> cond

  val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond

  val dual : cmpG -> ZTerm.term -> ZTerm.term -> t

  val nnf : t -> t

  val nnfNot : t -> t
 end
