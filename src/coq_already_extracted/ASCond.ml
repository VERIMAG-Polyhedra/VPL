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

module QCond =
 struct
  module Term = QTerm

  type cond =
  | Basic of bool
  | Atom of cmpG * QTerm.term * QTerm.term
  | BinL of binl * cond * cond
  | Not of cond

  (** val cond_rect :
      (bool -> 'a1) -> (cmpG -> QTerm.term -> QTerm.term -> 'a1) -> (binl ->
      cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond ->
      'a1 **)

  let rec cond_rect f f0 f1 f2 = function
  | Basic b -> f b
  | Atom (oc, tl, tr) -> f0 oc tl tr
  | BinL (op, cl, cr) ->
    f1 op cl (cond_rect f f0 f1 f2 cl) cr (cond_rect f f0 f1 f2 cr)
  | Not c0 -> f2 c0 (cond_rect f f0 f1 f2 c0)

  (** val cond_rec :
      (bool -> 'a1) -> (cmpG -> QTerm.term -> QTerm.term -> 'a1) -> (binl ->
      cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond ->
      'a1 **)

  let rec cond_rec f f0 f1 f2 = function
  | Basic b -> f b
  | Atom (oc, tl, tr) -> f0 oc tl tr
  | BinL (op, cl, cr) ->
    f1 op cl (cond_rec f f0 f1 f2 cl) cr (cond_rec f f0 f1 f2 cr)
  | Not c0 -> f2 c0 (cond_rec f f0 f1 f2 c0)

  type t = cond

  (** val sat_dec : t -> QNum.t Mem.t -> bool **)

  let rec sat_dec c m =
    match c with
    | Basic b -> if b then true else false
    | Atom (oc, tl, tr) ->
      (match oc with
       | Eq -> coq_Qc_eq_dec (QTerm.eval tl m) (QTerm.eval tr m)
       | Le ->
         if coq_Qclt_le_dec (QTerm.eval tr m) (QTerm.eval tl m)
         then false
         else true
       | Lt -> coq_Qclt_le_dec (QTerm.eval tl m) (QTerm.eval tr m)
       | Neq ->
         if coq_Qc_eq_dec (QTerm.eval tl m) (QTerm.eval tr m)
         then false
         else true)
    | BinL (op, cl, cr) ->
      (match op with
       | AND -> if sat_dec cl m then sat_dec cr m else false
       | OR -> if sat_dec cl m then true else sat_dec cr m)
    | Not c0 -> if sat_dec c0 m then false else true

  (** val xsat_dec : t -> QNum.t Mem.t -> QNum.t Mem.t -> bool **)

  let rec xsat_dec c old new0 =
    match c with
    | Basic b -> if b then true else false
    | Atom (oc, tl, tr) ->
      (match oc with
       | Eq ->
         coq_Qc_eq_dec (QTerm.xeval tl old new0) (QTerm.xeval tr old new0)
       | Le ->
         if coq_Qclt_le_dec (QTerm.xeval tr old new0)
              (QTerm.xeval tl old new0)
         then false
         else true
       | Lt ->
         coq_Qclt_le_dec (QTerm.xeval tl old new0) (QTerm.xeval tr old new0)
       | Neq ->
         if coq_Qc_eq_dec (QTerm.xeval tl old new0) (QTerm.xeval tr old new0)
         then false
         else true)
    | BinL (op, cl, cr) ->
      (match op with
       | AND -> if xsat_dec cl old new0 then xsat_dec cr old new0 else false
       | OR -> if xsat_dec cl old new0 then true else xsat_dec cr old new0)
    | Not c0 -> if xsat_dec c0 old new0 then false else true

  (** val mdBound : cond -> positive -> positive **)

  let rec mdBound c bound =
    match c with
    | Basic _ -> bound
    | Atom (_, tl, tr) -> QTerm.mdBound tl (QTerm.mdBound tr bound)
    | BinL (_, cl, cr) -> mdBound cl (mdBound cr bound)
    | Not c0 -> mdBound c0 bound

  (** val map : cond -> PVar.t Mem.t -> cond **)

  let rec map c f =
    match c with
    | Basic _ -> c
    | Atom (oc, tl, tr) -> Atom (oc, (QTerm.map tl f), (QTerm.map tr f))
    | BinL (op, cl, cr) -> BinL (op, (map cl f), (map cr f))
    | Not c0 -> Not (map c0 f)

  (** val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond **)

  let rec xmap c old new0 =
    match c with
    | Basic _ -> c
    | Atom (oc, tl, tr) ->
      Atom (oc, (QTerm.xmap tl old new0), (QTerm.xmap tr old new0))
    | BinL (op, cl, cr) -> BinL (op, (xmap cl old new0), (xmap cr old new0))
    | Not c0 -> Not (xmap c0 old new0)

  (** val dual : cmpG -> QTerm.term -> QTerm.term -> t **)

  let dual cmp t1 t2 =
    match cmp with
    | Eq -> Atom (Neq, t1, t2)
    | Le -> Atom (Lt, t2, t1)
    | Lt -> Atom (Le, t2, t1)
    | Neq -> Atom (Eq, t1, t2)

  (** val nnf : t -> t **)

  let rec nnf c = match c with
  | Basic _ -> c
  | Atom (op, tl, tr) -> Atom (op, tl, tr)
  | BinL (op, cl, cr) -> BinL (op, (nnf cl), (nnf cr))
  | Not c0 -> nnfNot c0

  (** val nnfNot : t -> t **)

  and nnfNot = function
  | Basic b -> Basic (negb b)
  | Atom (op, tl, tr) ->
    (match op with
     | Eq -> Atom (Neq, tl, tr)
     | Le -> Atom (Lt, tr, tl)
     | Lt -> Atom (Le, tr, tl)
     | Neq -> Atom (Eq, tl, tr))
  | BinL (op, cl, cr) ->
    (match op with
     | AND -> BinL (OR, (nnfNot cl), (nnfNot cr))
     | OR -> BinL (AND, (nnfNot cl), (nnfNot cr)))
  | Not c0 -> nnf c0
 end

module ZCond =
 struct
  module Term = ZTerm

  type cond =
  | Basic of bool
  | Atom of cmpG * ZTerm.term * ZTerm.term
  | BinL of binl * cond * cond
  | Not of cond

  (** val cond_rect :
      (bool -> 'a1) -> (cmpG -> ZTerm.term -> ZTerm.term -> 'a1) -> (binl ->
      cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond ->
      'a1 **)

  let rec cond_rect f f0 f1 f2 = function
  | Basic b -> f b
  | Atom (oc, tl, tr) -> f0 oc tl tr
  | BinL (op, cl, cr) ->
    f1 op cl (cond_rect f f0 f1 f2 cl) cr (cond_rect f f0 f1 f2 cr)
  | Not c0 -> f2 c0 (cond_rect f f0 f1 f2 c0)

  (** val cond_rec :
      (bool -> 'a1) -> (cmpG -> ZTerm.term -> ZTerm.term -> 'a1) -> (binl ->
      cond -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond ->
      'a1 **)

  let rec cond_rec f f0 f1 f2 = function
  | Basic b -> f b
  | Atom (oc, tl, tr) -> f0 oc tl tr
  | BinL (op, cl, cr) ->
    f1 op cl (cond_rec f f0 f1 f2 cl) cr (cond_rec f f0 f1 f2 cr)
  | Not c0 -> f2 c0 (cond_rec f f0 f1 f2 c0)

  type t = cond

  (** val sat_dec : t -> ZNum.t Mem.t -> bool **)

  let rec sat_dec c m =
    match c with
    | Basic b -> if b then true else false
    | Atom (oc, tl, tr) ->
      (match oc with
       | Eq -> Z.eq_dec (ZTerm.eval tl m) (ZTerm.eval tr m)
       | Le ->
         if coq_Z_lt_le_dec (ZTerm.eval tr m) (ZTerm.eval tl m)
         then false
         else true
       | Lt -> coq_Z_lt_le_dec (ZTerm.eval tl m) (ZTerm.eval tr m)
       | Neq ->
         if Z.eq_dec (ZTerm.eval tl m) (ZTerm.eval tr m) then false else true)
    | BinL (op, cl, cr) ->
      (match op with
       | AND -> if sat_dec cl m then sat_dec cr m else false
       | OR -> if sat_dec cl m then true else sat_dec cr m)
    | Not c0 -> if sat_dec c0 m then false else true

  (** val xsat_dec : t -> ZNum.t Mem.t -> ZNum.t Mem.t -> bool **)

  let rec xsat_dec c old new0 =
    match c with
    | Basic b -> if b then true else false
    | Atom (oc, tl, tr) ->
      (match oc with
       | Eq -> Z.eq_dec (ZTerm.xeval tl old new0) (ZTerm.xeval tr old new0)
       | Le ->
         if coq_Z_lt_le_dec (ZTerm.xeval tr old new0)
              (ZTerm.xeval tl old new0)
         then false
         else true
       | Lt ->
         coq_Z_lt_le_dec (ZTerm.xeval tl old new0) (ZTerm.xeval tr old new0)
       | Neq ->
         if Z.eq_dec (ZTerm.xeval tl old new0) (ZTerm.xeval tr old new0)
         then false
         else true)
    | BinL (op, cl, cr) ->
      (match op with
       | AND -> if xsat_dec cl old new0 then xsat_dec cr old new0 else false
       | OR -> if xsat_dec cl old new0 then true else xsat_dec cr old new0)
    | Not c0 -> if xsat_dec c0 old new0 then false else true

  (** val mdBound : cond -> positive -> positive **)

  let rec mdBound c bound =
    match c with
    | Basic _ -> bound
    | Atom (_, tl, tr) -> ZTerm.mdBound tl (ZTerm.mdBound tr bound)
    | BinL (_, cl, cr) -> mdBound cl (mdBound cr bound)
    | Not c0 -> mdBound c0 bound

  (** val map : cond -> PVar.t Mem.t -> cond **)

  let rec map c f =
    match c with
    | Basic _ -> c
    | Atom (oc, tl, tr) -> Atom (oc, (ZTerm.map tl f), (ZTerm.map tr f))
    | BinL (op, cl, cr) -> BinL (op, (map cl f), (map cr f))
    | Not c0 -> Not (map c0 f)

  (** val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond **)

  let rec xmap c old new0 =
    match c with
    | Basic _ -> c
    | Atom (oc, tl, tr) ->
      Atom (oc, (ZTerm.xmap tl old new0), (ZTerm.xmap tr old new0))
    | BinL (op, cl, cr) -> BinL (op, (xmap cl old new0), (xmap cr old new0))
    | Not c0 -> Not (xmap c0 old new0)

  (** val dual : cmpG -> ZTerm.term -> ZTerm.term -> t **)

  let dual cmp t1 t2 =
    match cmp with
    | Eq -> Atom (Neq, t1, t2)
    | Le -> Atom (Lt, t2, t1)
    | Lt -> Atom (Le, t2, t1)
    | Neq -> Atom (Eq, t1, t2)

  (** val nnf : t -> t **)

  let rec nnf c = match c with
  | Basic _ -> c
  | Atom (op, tl, tr) -> Atom (op, tl, tr)
  | BinL (op, cl, cr) -> BinL (op, (nnf cl), (nnf cr))
  | Not c0 -> nnfNot c0

  (** val nnfNot : t -> t **)

  and nnfNot = function
  | Basic b -> Basic (negb b)
  | Atom (op, tl, tr) ->
    (match op with
     | Eq -> Atom (Neq, tl, tr)
     | Le -> Atom (Lt, tr, tl)
     | Lt -> Atom (Le, tr, tl)
     | Neq -> Atom (Eq, tl, tr))
  | BinL (op, cl, cr) ->
    (match op with
     | AND -> BinL (OR, (nnfNot cl), (nnfNot cr))
     | OR -> BinL (AND, (nnfNot cl), (nnfNot cr)))
  | Not c0 -> nnf c0
 end
