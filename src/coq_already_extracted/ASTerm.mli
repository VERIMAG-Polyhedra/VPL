open BinInt
open BinNums
open Equalities
open LinTerm
open NumC
open ProgVar
open QArith_base
open Qcanon
open Ring_polynom
open Ring_polynom_AddOn
open Ring_polynom_AddOnQ
open String0
open ZNoneItv
open Zbool

module ZTerm2Pomial :
 functor (ATerm:sig
  module Annot :
   Typ

  type term =
  | Var of PVar.t
  | Cte of ZNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.t * term

  val term_rect :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

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

  val smartAnnot : Annot.t -> term -> term

  val import_acc : (PVar.t*ZNum.t) list -> term -> term

  val import : (PVar.t*ZNum.t) list -> term
 end) ->
 sig
  val toPExpr : ATerm.term -> Ring_polynom_AddOn.coq_PExpr
 end

module ZPomialEquality :
 functor (ATerm1:sig
  module Annot :
   Typ

  type term =
  | Var of PVar.t
  | Cte of ZNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.t * term

  val term_rect :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

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

  val smartAnnot : Annot.t -> term -> term

  val import_acc : (PVar.t*ZNum.t) list -> term -> term

  val import : (PVar.t*ZNum.t) list -> term
 end) ->
 functor (ATerm2:sig
  module Annot :
   Typ

  type term =
  | Var of PVar.t
  | Cte of ZNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.t * term

  val term_rect :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

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

  val smartAnnot : Annot.t -> term -> term

  val import_acc : (PVar.t*ZNum.t) list -> term -> term

  val import : (PVar.t*ZNum.t) list -> term
 end) ->
 sig
  module M1 :
   sig
    val toPExpr : ATerm1.term -> Ring_polynom_AddOn.coq_PExpr
   end

  module M2 :
   sig
    val toPExpr : ATerm2.term -> Ring_polynom_AddOn.coq_PExpr
   end

  val pomial_eq : ATerm1.t -> ATerm2.t -> bool
 end

module QTerm2Pomial :
 functor (ATerm:sig
  module Annot :
   Typ

  type term =
  | Var of PVar.t
  | Cte of QNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.t * term

  val term_rect :
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.t -> term -> 'a1 -> 'a1) -> term -> 'a1

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

  val smartAnnot : Annot.t -> term -> term

  val import_acc : (PVar.t*QNum.t) list -> term -> term

  val import : (PVar.t*QNum.t) list -> term
 end) ->
 sig
  val toPExpr : ATerm.term -> coq_PExpr
 end

module TopLevelAnnot :
 sig
  type topLevelAnnot =
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

module ModalTerm :
 functor (N:NumSig) ->
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

  type term =
  | Var of PVar.t
  | Cte of N.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  val term_rect :
    (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  type t = term

  val eval : term -> N.t Mem.t -> N.t

  val mdBound : term -> PVar.t -> PVar.t

  val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1

  val map : term -> PVar.t Mem.t -> term

  val pseudoIsZero : term -> bool

  val smartScalAdd1 : N.t -> term -> term

  val smartScalAdd : N.t -> term -> term

  val smartAdd : term -> term -> term

  val smartOpp : term -> term

  val smartScalMul1 : N.t -> term -> term

  val smartScalMul : N.t -> term -> term

  val smartMul : term -> term -> term

  val smartAnnot : Annot.topLevelAnnot -> term -> term

  val import_acc : (PVar.t*N.t) list -> term -> term

  val import : (PVar.t*N.t) list -> term

  val coq_Old : term -> term

  val xeval : term -> N.t Mem.t -> N.t Mem.t -> N.t

  val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term

  val isCte : term -> bool

  val annotAFFINEx : term -> term

  val annotAFFINE_rec : term -> term option

  val annotAFFINE : term -> term

  val matchCte : term -> N.t option

  val pr : term -> char list
 end

module BasicQTerm :
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

  type term =
  | Var of PVar.t
  | Cte of QNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  val term_rect :
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
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
 end

module QTerm :
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
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
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

module BasicZTerm :
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

  type term =
  | Var of PVar.t
  | Cte of ZNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  val term_rect :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
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
 end

module ZTerm :
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
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
    (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

  val term_rec :
    (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
    -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
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

type linearizeContext = { nonaffine : ZTerm.t; env : (PVar.t -> ZNItv.t);
                          affine : ZAffTerm.t; source : ZTerm.t; cmp : 
                          cmpG }
