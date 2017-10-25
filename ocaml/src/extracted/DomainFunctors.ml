open ASAtomicCond
open ASCond
open ASTerm
open BinInt
open BinNums
open CstrC
open Datatypes
open Debugging
open DomainInterfaces
open FMapPositive
open ImpureConfig
open Itv
open LinTerm
open LinearizeBackend
open MSetPositive
open NumC
open PredTrans
open ProgVar
open Ring_polynom_AddOn
open String0
open ZNone
open ZNoneItv

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module MakeFull =
 functor (N:NumSig) ->
 functor (Cond:sig
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

    type term =
    | Var of PVar.t
    | Cte of N.t
    | Add of term * term
    | Opp of term
    | Mul of term * term
    | Annot of Annot.topLevelAnnot * term

    val term_rect :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1

    val term_rec :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
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

  type cond =
  | Basic of bool
  | Atom of cmpG * Term.term * Term.term
  | BinL of binl * cond * cond
  | Not of cond

  val cond_rect :
    (bool -> 'a1) -> (cmpG -> Term.term -> Term.term -> 'a1) -> (binl -> cond
    -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  val cond_rec :
    (bool -> 'a1) -> (cmpG -> Term.term -> Term.term -> 'a1) -> (binl -> cond
    -> 'a1 -> cond -> 'a1 -> 'a1) -> (cond -> 'a1 -> 'a1) -> cond -> 'a1

  type t = cond

  val sat_dec : t -> N.t Mem.t -> bool

  val xsat_dec : t -> N.t Mem.t -> N.t Mem.t -> bool

  val mdBound : cond -> positive -> positive

  val map : cond -> PVar.t Mem.t -> cond

  val xmap : cond -> PVar.t Mem.t -> PVar.t Mem.t -> cond

  val dual : cmpG -> Term.term -> Term.term -> t

  val nnf : t -> t

  val nnfNot : t -> t
 end) ->
 functor (I:sig
  type t
 end) ->
 functor (AtomC:sig
  type atomicCond = { cmpOp : cmpG; right : Cond.Term.t }

  val cmpOp : atomicCond -> cmpG

  val right : atomicCond -> Cond.Term.t

  type t = atomicCond

  val make : Cond.Term.term -> cmpG -> Cond.Term.term -> t
 end) ->
 functor (D:sig
  type t

  val isIncl : t -> t -> bool Core.Base.imp

  val top : t

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp
 end) ->
 functor (AtomD:sig
  val assume : AtomC.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (R:sig
  val rename : PVar.t -> PVar.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (DI:sig
  val getItvMode : mode -> Cond.Term.t -> D.t -> I.t Core.Base.imp
 end) ->
 functor (DP:sig
  val pr : D.t -> char list

  val to_string : (PVar.t -> char list) -> D.t -> char list

  type rep

  val backend_rep :
    D.t -> (rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

  val meet : D.t -> D.t -> D.t Core.Base.imp
 end) ->
 struct
  module Dom =
   struct
    type t = D.t

    (** val isIncl : t -> t -> bool Core.Base.imp **)

    let isIncl =
      D.isIncl

    (** val top : t **)

    let top =
      D.top

    (** val join : t -> t -> t Core.Base.imp **)

    let join =
      D.join

    (** val widen : t -> t -> t Core.Base.imp **)

    let widen =
      D.widen

    (** val bottom : t **)

    let bottom =
      D.bottom

    (** val isBottom : t -> bool Core.Base.imp **)

    let isBottom =
      D.isBottom

    (** val project : t -> PVar.t -> t Core.Base.imp **)

    let project =
      D.project

    (** val skipBottom : D.t -> D.t Core.Base.imp -> D.t Core.Base.imp **)

    let skipBottom a k =
      if D.isBottom a then a else k

    (** val assumeRec : Cond.t -> D.t -> D.t Core.Base.imp **)

    let rec assumeRec c a =
      match c with
      | Cond.Basic b -> if b then a else D.bottom
      | Cond.Atom (cmp0, tl, tr) -> AtomD.assume (AtomC.make tl cmp0 tr) a
      | Cond.BinL (op, cl, cr) ->
        (match op with
         | AND ->
           let aux = assumeRec cl a in skipBottom aux (assumeRec cr aux)
         | OR -> D.join (assumeRec cl a) (assumeRec cr a))
      | Cond.Not _ ->
        (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
          INTERN
          ('a'::('s'::('s'::('u'::('m'::('e'::(':'::('N'::('o'::('t'::[]))))))))))
          D.top

    (** val assume : Cond.cond -> D.t -> D.t Core.Base.imp **)

    let assume c a =
      skipBottom a (assumeRec (Cond.nnf c) a)

    module Naive =
     struct
      (** val coq_assert : Cond.cond -> t -> bool Core.Base.imp **)

      let coq_assert c a =
        isBottom (assume (Cond.Not c) a)
     end

    (** val assertRec : Cond.t -> t -> bool Core.Base.imp **)

    let rec assertRec c a =
      match c with
      | Cond.Basic b -> if b then true else isBottom a
      | Cond.Atom (oc, tl, tr) ->
        (match oc with
         | Eq ->
           if Naive.coq_assert (Cond.Atom (Le, tl, tr)) a
           then Naive.coq_assert (Cond.Atom (Le, tr, tl)) a
           else false
         | _ -> Naive.coq_assert c a)
      | Cond.BinL (op, cl, cr) ->
        (match op with
         | AND -> if assertRec cl a then assertRec cr a else false
         | OR -> Naive.coq_assert c a)
      | Cond.Not _ -> Naive.coq_assert c a

    (** val coq_assert : Cond.t -> t -> bool Core.Base.imp **)

    let coq_assert c a =
      assertRec (Cond.nnf c) a
   end

  (** val encode : bool -> positive -> positive **)

  let encode b p =
    if b then Coq_xI p else Coq_xO p

  (** val decode : positive -> positive **)

  let decode = function
  | Coq_xI p0 -> p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH

  (** val encodeE : PositiveSet.t -> positive -> positive **)

  let encodeE r x =
    encode (PositiveSet.mem x r) x

  (** val encodeO : PositiveSet.t -> positive -> positive **)

  let encodeO r x =
    encode (negb (PositiveSet.mem x r)) x

  (** val decodeIn :
      PositiveSet.t -> N.t Mem.t -> N.t Mem.t -> positive -> N.t **)

  let decodeIn r aux m = function
  | Coq_xI p0 -> if PositiveSet.mem p0 r then m p0 else aux (Coq_xI p0)
  | Coq_xO p0 -> if PositiveSet.mem p0 r then aux (Coq_xO p0) else m p0
  | Coq_xH -> aux Coq_xH

  (** val switch : PositiveSet.t -> positive -> PositiveSet.t **)

  let rec switch m i =
    match m with
    | PositiveSet.Leaf -> PositiveSet.add i PositiveSet.Leaf
    | PositiveSet.Node (l, o, r) ->
      (match i with
       | Coq_xI p -> PositiveSet.node l o (switch r p)
       | Coq_xO p -> PositiveSet.node (switch l p) o r
       | Coq_xH -> PositiveSet.node l (negb o) r)

  type assignDomain = { pol : Dom.t; renaming : PositiveSet.t }

  (** val pol : assignDomain -> Dom.t **)

  let pol a =
    a.pol

  (** val renaming : assignDomain -> PositiveSet.t **)

  let renaming a =
    a.renaming

  type t = assignDomain

  (** val top : t **)

  let top =
    { pol = Dom.top; renaming = PositiveSet.empty }

  (** val bottom : t **)

  let bottom =
    { pol = Dom.bottom; renaming = PositiveSet.empty }

  (** val isBottom : t -> bool Core.Base.imp **)

  let isBottom p =
    Dom.isBottom (pol p)

  (** val assume : Cond.cond -> t -> t Core.Base.imp **)

  let assume c a =
    { pol = (Dom.assume (Cond.map c (encodeE (renaming a))) (pol a));
      renaming = (renaming a) }

  (** val coq_assert : Cond.cond -> t -> bool Core.Base.imp **)

  let coq_assert c a =
    Dom.coq_assert (Cond.map c (encodeE (renaming a))) (pol a)

  (** val project : t -> PVar.t -> t Core.Base.imp **)

  let project a x =
    { pol = (Dom.project (pol a) (encodeE (renaming a) x)); renaming =
      (renaming a) }

  (** val guassignAux : positive -> Cond.cond -> t -> t Core.Base.imp **)

  let guassignAux x c a =
    let a0 =
      (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
        DEBUG
        ('g'::('u'::('a'::('s'::('s'::('i'::('g'::('n'::(' '::('c'::('a'::('l'::('l'::('e'::('d'::[])))))))))))))))
        a
    in
    { pol = (Dom.project (Dom.assume c (pol a0)) (encodeE (renaming a0) x));
    renaming = (switch (renaming a0) x) }

  (** val guassign : PVar.t -> Cond.cond -> t -> t Core.Base.imp **)

  let guassign x c a =
    let f = encodeE (renaming a) in
    let c1 = Cond.xmap c f (Mem.assign x (encodeO (renaming a) x) f) in
    guassignAux x c1 a

  (** val assign : positive -> Cond.Term.term -> t -> t Core.Base.imp **)

  let assign x te a =
    let c = Cond.Atom (Eq, (Cond.Term.Var (encodeO (renaming a) x)),
      (Cond.Term.coq_Old (Cond.Term.map te (encodeE (renaming a)))))
    in
    guassignAux x c a

  (** val nop : PVar.t -> assignDomain -> t Core.Base.imp **)

  let nop x a =
    { pol =
      (R.rename (encodeE (renaming a) x) (encodeO (renaming a) x) (pol a));
      renaming = (switch (renaming a) x) }

  (** val switchAll : PositiveSet.t -> t -> t Core.Base.imp **)

  let switchAll r a =
    PositiveSet.fold nop r a

  (** val isIncl : t -> t -> bool Core.Base.imp **)

  let isIncl a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    Dom.isIncl (pol (switchAll r2 (switchAll r1 a1))) (pol a2)

  (** val join : t -> t -> t Core.Base.imp **)

  let join a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (Dom.join (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }

  (** val widen : t -> t -> t Core.Base.imp **)

  let widen a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (Dom.widen (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }

  (** val rename :
      positive -> positive -> assignDomain -> assignDomain Core.Base.imp **)

  let rename x y a =
    { pol =
      (R.rename (encodeE (renaming a) x) (encodeE (renaming a) y) (pol a));
      renaming = (renaming a) }

  (** val getItvMode : mode -> Cond.Term.t -> t -> I.t Core.Base.imp **)

  let getItvMode mo te a =
    DI.getItvMode mo (Cond.Term.map te (encodeE (renaming a))) (pol a)

  (** val pr : t -> char list **)

  let pr a =
    DP.pr (pol a)

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f a =
    DP.to_string (fun x -> f (decode x)) (pol a)

  type rep = DP.rep

  (** val backend_rep :
      t -> (DP.rep*((PVar.t -> positive)*(positive -> PVar.t))) option **)

  let backend_rep a =
    match DP.backend_rep (pol a) with
    | Some p ->
      let r,p0 = p in
      let fa,fu = p0 in
      Some (r,((fun x -> encodeE (renaming a) (fa x)),(fun x ->
      fu (decode x))))
    | None -> None

  (** val meet : t -> t -> t Core.Base.imp **)

  let meet a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (DP.meet (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }
 end

module MakeZ =
 functor (D:sig
  type t

  val isIncl : t -> t -> bool Core.Base.imp

  val top : t

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp
 end) ->
 functor (QCstrD:sig
  val assume : Cstr.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (QItvD:sig
  val getItvMode : mode -> QAffTerm.t -> D.t -> QItv.t Core.Base.imp
 end) ->
 functor (R:sig
  val rename : PVar.t -> PVar.t -> D.t -> D.t Core.Base.imp
 end) ->
 functor (DP:sig
  val pr : D.t -> char list

  val to_string : (PVar.t -> char list) -> D.t -> char list

  type rep

  val backend_rep :
    D.t -> (rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option

  val meet : D.t -> D.t -> D.t Core.Base.imp
 end) ->
 struct
  module BasicD =
   struct
    type t = D.t

    (** val isIncl : D.t -> D.t -> bool Core.Base.imp **)

    let isIncl =
      D.isIncl

    (** val top : D.t **)

    let top =
      D.top

    (** val bottom : t **)

    let bottom =
      D.bottom

    (** val isBottom : t -> bool Core.Base.imp **)

    let isBottom =
      D.isBottom

    (** val widen : D.t -> D.t -> D.t Core.Base.imp **)

    let widen =
      D.widen

    (** val join : D.t -> D.t -> D.t Core.Base.imp **)

    let join =
      D.join

    (** val project : t -> PVar.t -> t Core.Base.imp **)

    let project =
      D.project

    (** val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp **)

    let rename =
      R.rename

    (** val pr : t -> char list **)

    let pr =
      DP.pr

    (** val to_string : (PVar.t -> char list) -> D.t -> char list **)

    let to_string =
      DP.to_string

    type rep = DP.rep

    (** val backend_rep :
        D.t -> (DP.rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option **)

    let backend_rep =
      DP.backend_rep

    (** val meet : D.t -> D.t -> D.t Core.Base.imp **)

    let meet =
      DP.meet
   end

  module ZNItvD =
   struct
    (** val getItvMode :
        mode -> BasicZTerm.term -> BasicD.t -> ZNItv.itv Core.Base.imp **)

    let getItvMode mo te a =
      let te0,aft = ZTerm.affineDecompose te in
      (match te0 with
       | ZTerm.Cte c ->
         (match c with
          | Z0 -> ZNItv.fromQItv (QItvD.getItvMode mo (QAffTerm.lift aft) a)
          | _ ->
            (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              INTERN
              ('g'::('e'::('t'::('_'::('i'::('t'::('v'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[]))))))))))))))))))))))))
              ZNItv.top)
       | _ ->
         (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           INTERN
           ('g'::('e'::('t'::('_'::('i'::('t'::('v'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[]))))))))))))))))))))))))
           ZNItv.top)
   end

  module CstrD =
   struct
    (** val assume : ZtoQCstr.t -> BasicD.t -> BasicD.t Core.Base.imp **)

    let assume =
      QCstrD.assume
   end

  module AtomicD =
   struct
    type t = D.t

    (** val isIncl : D.t -> D.t -> bool Core.Base.imp **)

    let isIncl =
      D.isIncl

    (** val top : D.t **)

    let top =
      D.top

    (** val bottom : t **)

    let bottom =
      D.bottom

    (** val isBottom : t -> bool Core.Base.imp **)

    let isBottom =
      D.isBottom

    (** val widen : D.t -> D.t -> D.t Core.Base.imp **)

    let widen =
      D.widen

    (** val join : D.t -> D.t -> D.t Core.Base.imp **)

    let join =
      D.join

    (** val project : t -> PVar.t -> t Core.Base.imp **)

    let project =
      D.project

    (** val rename : PVar.t -> PVar.t -> t -> t Core.Base.imp **)

    let rename =
      R.rename

    (** val pr : t -> char list **)

    let pr =
      DP.pr

    (** val to_string : (PVar.t -> char list) -> D.t -> char list **)

    let to_string =
      DP.to_string

    type rep = DP.rep

    (** val backend_rep :
        D.t -> (DP.rep*((PVar.t -> PVar.t)*(PVar.t -> PVar.t))) option **)

    let backend_rep =
      DP.backend_rep

    (** val meet : D.t -> D.t -> D.t Core.Base.imp **)

    let meet =
      DP.meet

    (** val affAssumeLe : ZAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp **)

    let affAssumeLe aft a =
      CstrD.assume (ZAtomicCond.toCstr LeT aft) a

    (** val affAssumeLt :
        ZAffTerm.affTerm -> BasicD.t -> BasicD.t Core.Base.imp **)

    let affAssumeLt aft a =
      affAssumeLe (ZAffTerm.addc (Zneg Coq_xH) aft) a

    (** val affAssumeGt :
        ZAffTerm.affTerm -> BasicD.t -> BasicD.t Core.Base.imp **)

    let affAssumeGt aft a =
      affAssumeLt (ZAffTerm.opp aft) a

    (** val affAssume :
        cmpG -> ZAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp **)

    let affAssume cmp0 aft a =
      match cmp0 with
      | Eq -> CstrD.assume (ZAtomicCond.toCstr EqT aft) a
      | Le -> affAssumeLe aft a
      | Lt -> affAssumeLt aft a
      | Neq -> BasicD.join (affAssumeLt aft a) (affAssumeGt aft a)

    (** val add_variable :
        BasicD.t -> PVar.t -> ZNItv.t PositiveMap.t -> ZNItv.t PositiveMap.t
        Core.Base.imp **)

    let add_variable a x mitv =
      match PositiveMap.find x mitv with
      | Some _ -> mitv
      | None ->
        PositiveMap.add x (ZNItvD.getItvMode BOTH (ZTerm.Var x) a) mitv

    (** val get_variables :
        BasicD.t -> ZTerm.term -> ZNItv.t PositiveMap.t Core.Base.imp **)

    let get_variables a te =
      let rec fold_variables0 te0 f i =
        match te0 with
        | ZTerm.Var x -> f x i
        | ZTerm.Cte _ -> i
        | ZTerm.Add (tl, tr) -> fold_variables0 tl f (fold_variables0 tr f i)
        | ZTerm.Opp te1 -> fold_variables0 te1 f i
        | ZTerm.Mul (tl, tr) -> fold_variables0 tl f (fold_variables0 tr f i)
        | ZTerm.Annot (_, te1) -> fold_variables0 te1 f i
      in fold_variables0 te (fun x i -> add_variable a x i) PositiveMap.Leaf

    (** val mitv_find :
        ZNItv.t PositiveMap.t -> PositiveMap.key -> ZNItv.t **)

    let mitv_find mitv x =
      match PositiveMap.find x mitv with
      | Some itv0 -> itv0
      | None -> ZNItv.top

    (** val intervalize :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> BasicD.t ->
        ZNItv.t Core.Base.imp **)

    let rec intervalize env0 sic mo te a =
      match te with
      | ZTerm.Var x ->
        if sic then env0 x else ZNItvD.getItvMode mo (ZTerm.Var x) a
      | ZTerm.Cte c -> ZNItv.single c
      | ZTerm.Add (tl, tr) ->
        ZNItv.add mo (intervalize env0 sic mo tl a)
          (intervalize env0 sic mo tr a)
      | ZTerm.Opp te0 ->
        ZNItv.opp (intervalize env0 sic (ZNItv.oppMode mo) te0 a)
      | ZTerm.Mul (tl, tr) ->
        (match let rec matchCte0 = function
               | ZTerm.Cte c -> Some c
               | ZTerm.Annot (_, te1) -> matchCte0 te1
               | _ -> None
               in matchCte0 tr with
         | Some c ->
           let i =
             intervalize env0 sic
               (if coq_Z_isNat c then mo else ZNItv.oppMode mo) tl a
           in
           if coq_Z_isNat c
           then (match mo with
                 | BOTH ->
                   { ZNItv.low =
                     (if coq_Z_isZero c
                      then Some Z0
                      else (match i.ZNItv.low with
                            | Some z2 -> Some (Z.mul c z2)
                            | None -> None)); ZNItv.up =
                     (if coq_Z_isZero c
                      then Some Z0
                      else (match i.ZNItv.up with
                            | Some z2 -> Some (Z.mul c z2)
                            | None -> None)) }
                 | UP ->
                   { ZNItv.low = None; ZNItv.up =
                     (if coq_Z_isZero c
                      then Some Z0
                      else (match i.ZNItv.up with
                            | Some z2 -> Some (Z.mul c z2)
                            | None -> None)) }
                 | LOW ->
                   { ZNItv.low =
                     (if coq_Z_isZero c
                      then Some Z0
                      else (match i.ZNItv.low with
                            | Some z2 -> Some (Z.mul c z2)
                            | None -> None)); ZNItv.up = None })
           else (match mo with
                 | BOTH ->
                   { ZNItv.low =
                     (match i.ZNItv.up with
                      | Some z2 -> Some (Z.mul c z2)
                      | None -> None); ZNItv.up =
                     (match i.ZNItv.low with
                      | Some z2 -> Some (Z.mul c z2)
                      | None -> None) }
                 | UP ->
                   { ZNItv.low = None; ZNItv.up =
                     (match i.ZNItv.low with
                      | Some z2 -> Some (Z.mul c z2)
                      | None -> None) }
                 | LOW ->
                   { ZNItv.low =
                     (match i.ZNItv.up with
                      | Some z2 -> Some (Z.mul c z2)
                      | None -> None); ZNItv.up = None })
         | None ->
           ZNItv.mul mo (intervalize env0 sic BOTH tl a)
             (intervalize env0 sic BOTH tr a))
      | ZTerm.Annot (a0, te0) ->
        (match a0 with
         | ZTerm.Annot.AFFINE ->
           if sic
           then intervalize env0 sic mo te0 a
           else ZNItvD.getItvMode mo te0 a
         | ZTerm.Annot.STATIC ->
           let te1 =
             (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
               DEBUG
               ('!'::('I'::('N'::('T'::('E'::('R'::('V'::(' '::('S'::('T'::('A'::('T'::('I'::('C'::('!'::[])))))))))))))))
               te0
           in
           intervalize env0 true mo te1 a
         | _ -> intervalize env0 sic mo te0 a)

    module G =
     struct
      type cdac =
        BasicD.t -> BasicD.t Core.Base.imp
        (* singleton inductive, whose constructor was raw_build_cdac *)

      (** val impl : cdac -> BasicD.t -> BasicD.t Core.Base.imp **)

      let impl c =
        c

      (** val build_cdac :
          (BasicD.t -> BasicD.t Core.Base.imp) -> (ZNum.t Mem.t -> ZNum.t
          Mem.t MPP_Definitions.coq_MPP) -> cdac **)

      let build_cdac impl0 _ =
        impl0

      (** val spec :
          cdac -> ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP **)

      let spec _ _ =
        MPP_Definitions.Build_MPP

      (** val cast :
          cdac -> (ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP) ->
          cdac **)

      let cast gc spec' =
        build_cdac (impl gc) spec'

      (** val skip : cdac **)

      let skip =
        build_cdac (fun a -> a) coq_Skip

      (** val fail : char list -> cdac **)

      let fail msg =
        build_cdac (fun a ->
          (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
            DEMO msg a) coq_Skip

      (** val seq : cdac -> cdac -> cdac **)

      let seq gc1 gc2 =
        build_cdac (fun a ->
          let a' = impl gc1 a in
          if BasicD.isBottom a' then a' else impl gc2 a')
          (coq_Seq (spec gc1) (spec gc2))

      (** val join : cdac -> cdac -> cdac **)

      let join gc1 gc2 =
        build_cdac (fun a -> BasicD.join (impl gc1 a) (impl gc2 a))
          (coq_Join (spec gc1) (spec gc2))

      (** val loop : cdac -> (BasicD.t -> BasicD.t Core.Base.imp) -> cdac **)

      let loop gc oracle0 =
        build_cdac (fun a ->
          let inv = oracle0 a in
          if BasicD.isIncl a inv
          then if BasicD.isIncl (impl gc inv) inv
               then inv
               else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                      DEMO
                      ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('p'::('r'::('e'::('s'::('e'::('r'::('v'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
                      BasicD.top
          else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                 DEMO
                 ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('i'::('n'::('i'::('t'::[]))))))))))))))
                 BasicD.top)
          (coq_UMeet (fun _ ->
            coq_Seq coq_Assert
              (coq_Seq coq_Assert
                (coq_UJoin (fun m ->
                  coq_Seq (coq_Update (fun _ -> m)) coq_Assume)))))

      (** val skip_info : char list **)

      let skip_info =
        '#'::('s'::('k'::('i'::('p'::('#'::[])))))

      (** val coq_try : 'a1 option -> ('a1 -> cdac) -> cdac **)

      let coq_try o gc =
        build_cdac (fun a ->
          match o with
          | Some x -> impl (gc x) a
          | None ->
            (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
              INFO skip_info a) (coq_Try o (fun x -> spec (gc x)) coq_Skip)

      (** val bind :
          (BasicD.t -> 'a1 Core.Base.imp) -> ('a1 -> cdac) -> __ -> cdac **)

      let bind ge gc _ =
        build_cdac (fun a -> impl (gc (ge a)) a)
          (coq_UMeet (fun x -> coq_Seq coq_Assert (spec (gc x))))
     end

    (** val gAffAssumeLe : ZAffTerm.t -> G.cdac **)

    let gAffAssumeLe aft =
      G.build_cdac (affAssumeLe aft) coq_Assume

    (** val gAffAssumeLt : ZAffTerm.affTerm -> G.cdac **)

    let gAffAssumeLt aft =
      G.build_cdac (affAssumeLt aft) coq_Assume

    (** val gAffAssumeGt : ZAffTerm.affTerm -> G.cdac **)

    let gAffAssumeGt aft =
      G.build_cdac (affAssumeGt aft) coq_Assume

    (** val gIntervalize :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (ZNItv.t ->
        G.cdac) -> G.cdac **)

    let gIntervalize env0 sic mo te k =
      G.bind (intervalize env0 sic mo te) k __

    (** val intervalizeG :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.t -> (NAItv.itv ->
        G.cdac) -> G.cdac **)

    let intervalizeG env0 sic mo te k =
      G.cast (gIntervalize env0 sic mo te (fun i -> k (NAItv.cte i)))
        (coq_Seq coq_Assume
          (coq_UMeet (fun itv0 -> coq_Seq coq_Assert (G.spec (k itv0)))))

    (** val castAFFINE_error : char list **)

    let castAFFINE_error =
      'A'::('S'::('A'::('t'::('o'::('m'::('i'::('c'::('C'::('o'::('n'::('d'::('.'::('c'::('a'::('s'::('t'::('A'::('F'::('F'::('I'::('N'::('E'::(' '::('o'::('v'::('e'::('r'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::(' '::[]))))))))))))))))))))))))))))))))))))))))))))

    (** val castAFFINE :
        BasicZTerm.term -> (ZAffTerm.t -> G.cdac) -> G.cdac **)

    let castAFFINE te k =
      G.cast
        (let te',aft = ZTerm.affineDecompose te in
         (match te' with
          | ZTerm.Cte c ->
            (match c with
             | Z0 -> k aft
             | _ -> G.fail (append castAFFINE_error (ZTerm.pr te)))
          | _ -> G.fail (append castAFFINE_error (ZTerm.pr te))))
        (coq_Meet coq_Skip
          (coq_UMeet (fun aft -> coq_Seq coq_Assert (G.spec (k aft)))))

    (** val caseSign :
        BasicZTerm.term -> (ZAffTerm.t -> G.cdac) -> (ZAffTerm.t -> G.cdac)
        -> G.cdac **)

    let caseSign te aP aN =
      castAFFINE te (fun aft ->
        G.join (G.seq (gAffAssumeLe aft) (aP aft))
          (G.seq (gAffAssumeGt aft) (aN aft)))

    (** val linearizeMul :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> ZTerm.term ->
        (NAItv.itv -> G.cdac) -> G.cdac **)

    let linearizeMul env0 sic mo tl tr k =
      let omo = ZNItv.oppMode mo in
      G.cast
        (if sic
         then gIntervalize env0 true BOTH tl (fun itv0 ->
                caseSign tr (fun aftr -> k (NAItv.mulP1 mo itv0 aftr))
                  (fun aftr -> k (NAItv.mulN mo itv0 aftr)))
         else caseSign tr (fun aftr ->
                gIntervalize env0 sic mo tl (fun itv0 ->
                  k (NAItv.mulP1 mo itv0 aftr))) (fun aftr ->
                gIntervalize env0 sic omo tl (fun itv0 ->
                  k (NAItv.mulN mo itv0 aftr))))
        (coq_Seq coq_Assume
          (coq_Meet coq_Skip
            (coq_UMeet (fun itv0 -> coq_Seq coq_Assert (G.spec (k itv0))))))

    (** val linearizeG :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (NAItv.itv ->
        G.cdac) -> G.cdac **)

    let rec linearizeG env0 sic mo te k =
      match te with
      | ZTerm.Add (tl, tr) ->
        linearizeG env0 sic mo tl (fun itvl ->
          linearizeG env0 sic mo tr (fun itvr -> k (NAItv.add mo itvl itvr)))
      | ZTerm.Opp te0 ->
        linearizeG env0 sic (ZNItv.oppMode mo) te0 (fun itv0 ->
          k (NAItv.opp mo itv0))
      | ZTerm.Mul (tl, tr0) ->
        (match tr0 with
         | ZTerm.Annot (a, tr) ->
           (match a with
            | ZTerm.Annot.AFFINE ->
              (match let rec matchCte0 = function
                     | ZTerm.Cte c -> Some c
                     | ZTerm.Annot (_, te1) -> matchCte0 te1
                     | _ -> None
                     in matchCte0 tr with
               | Some c ->
                 linearizeG env0 sic
                   (if coq_Z_isNat c then mo else ZNItv.oppMode mo) tl
                   (fun itv0 -> k (NAItv.mulZ mo c itv0))
               | None -> linearizeMul env0 sic mo tl tr k)
            | _ -> intervalizeG env0 sic mo te k)
         | _ -> intervalizeG env0 sic mo te k)
      | ZTerm.Annot (a, te0) ->
        (match a with
         | ZTerm.Annot.AFFINE ->
           castAFFINE te0 (fun aft -> k (NAItv.single aft))
         | ZTerm.Annot.INTERV -> intervalizeG env0 sic mo te0 k
         | ZTerm.Annot.STATIC -> linearizeG env0 true mo te0 k
         | _ -> linearizeG env0 sic mo te0 k)
      | _ -> intervalizeG env0 sic mo te k

    (** val linearizeGX :
        (PVar.t -> ZNItv.t) -> bool -> mode -> ZTerm.term -> (NAItv.itv ->
        G.cdac) -> G.cdac **)

    let linearizeGX env0 sic mo te k =
      G.cast (linearizeG env0 sic mo te k)
        (coq_Seq coq_Assume
          (coq_Meet coq_Skip
            (coq_UMeet (fun itv0 -> coq_Seq coq_Assert (G.spec (k itv0))))))

    (** val assumeOpCPS :
        (PVar.t -> ZNItv.t) -> bool -> cmpG -> ZTerm.term -> ZAffTerm.affTerm
        -> G.cdac **)

    let assumeOpCPS env0 sic cmp0 te aft =
      let te0 =
        (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
          DEBUG
          (append
            ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('t'::('e'::(':'::[]))))))))))))
            (ZTerm.pr te)) te
      in
      let te1 =
        (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
          DEBUG
          (append
            ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('a'::('f'::('t'::(':'::[])))))))))))))
            (ZTerm.pr
              (ZTerm.smartAdd (ZTerm.fromLin aft.ZAffTerm.lin) (ZTerm.Cte
                aft.ZAffTerm.cte)))) te0
      in
      G.cast
        (match cmp0 with
         | Eq ->
           linearizeGX env0 sic BOTH te1 (fun i ->
             G.seq
               (G.coq_try i.NAItv.up (fun u ->
                 gAffAssumeLe (ZAffTerm.add u aft)))
               (G.coq_try i.NAItv.low (fun l ->
                 gAffAssumeLe (ZAffTerm.opp (ZAffTerm.add l aft)))))
         | Le ->
           linearizeGX env0 sic UP te1 (fun i ->
             G.coq_try i.NAItv.up (fun u -> gAffAssumeLe (ZAffTerm.add u aft)))
         | Lt ->
           linearizeGX env0 sic UP te1 (fun i ->
             G.coq_try i.NAItv.up (fun u -> gAffAssumeLt (ZAffTerm.add u aft)))
         | Neq ->
           linearizeGX env0 sic BOTH te1 (fun i ->
             G.join
               (G.coq_try i.NAItv.up (fun u ->
                 gAffAssumeLt (ZAffTerm.add u aft)))
               (G.coq_try i.NAItv.low (fun l ->
                 gAffAssumeGt (ZAffTerm.add l aft))))) coq_Assume

    (** val assumeOpAnnot :
        (PVar.t -> ZNItv.t) -> bool -> cmpG -> ZTerm.term -> ZAffTerm.affTerm
        -> G.cdac **)

    let assumeOpAnnot env0 sic cmp0 te aft =
      G.cast (assumeOpCPS env0 sic cmp0 (ZTerm.annotAFFINE te) aft) coq_Assume

    module ZPeq = ZPomialEquality(ZTerm)(ZTerm)

    (** val test_eq : ZTerm.t -> ZTerm.t -> ZTerm.t **)

    let test_eq te te' =
      if ZPeq.pomial_eq te te'
      then te'
      else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
             CERT
             ('L'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('e'::('O'::('r'::('a'::('c'::('l'::('e'::('B'::('u'::('g'::(' '::('?'::(' '::('T'::('h'::('e'::(' '::('t'::('w'::('o'::(' '::('p'::('o'::('l'::('y'::('n'::('o'::('m'::('i'::('a'::('l'::(' '::('d'::('i'::('f'::('f'::('e'::('r'::('s'::('.'::('.'::('.'::[]))))))))))))))))))))))))))))))))))))))))))))))))))
             te

    (** val skip_oracle : ZTerm.t -> bool **)

    let rec skip_oracle = function
    | ZTerm.Opp te0 -> skip_oracle te0
    | ZTerm.Annot (a, te0) ->
      (match a with
       | ZTerm.Annot.SKIP_ORACLE ->
         (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
           DEBUG
           ('O'::('R'::('A'::('C'::('L'::('E'::(' '::('S'::('K'::('I'::('P'::('P'::('E'::('D'::(' '::('!'::[]))))))))))))))))
           true
       | _ -> skip_oracle te0)
    | _ -> false

    (** val assumeOpFromOracle :
        (PVar.t -> ZNItv.t) -> bool -> linearizeContext -> cmpG -> ZTerm.t ->
        ZAffTerm.affTerm -> G.cdac **)

    let assumeOpFromOracle env0 sic lc cmp0 te aft =
      G.cast
        (G.bind (fun _ -> oracle lc) (fun te0 ->
          if skip_oracle te0
          then G.skip
          else let te' = test_eq te te0 in assumeOpAnnot env0 sic cmp0 te' aft)
          __) coq_Assume

    (** val assumeOp2 :
        (PVar.t -> ZNItv.t) -> bool -> linearizeContext -> cmpG -> ZTerm.term
        -> ZAffTerm.affTerm -> G.cdac **)

    let assumeOp2 env0 sic lc cmp0 te aft =
      G.cast
        (G.seq
          (assumeOpCPS env0 true cmp0 (ZTerm.Annot (TopLevelAnnot.INTERV,
            te)) aft) (assumeOpFromOracle env0 sic lc cmp0 te aft)) coq_Assume

    (** val assumeOp :
        bool -> cmpG -> ZTerm.t -> ZAffTerm.t -> ZTerm.t -> BasicD.t ->
        BasicD.t Core.Base.imp **)

    let assumeOp sic cmp0 te aft ti a =
      let env0 = mitv_find (get_variables a te) in
      let lc = { nonaffine = te; env = env0; affine = aft; source = ti; cmp =
        cmp0 }
      in
      let te0 =
        (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
          DEBUG
          (append
            ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('t'::('i'::(':'::[]))))))))))))
            (ZTerm.pr ti)) te
      in
      if skip_oracle te0
      then G.impl (assumeOpAnnot env0 sic cmp0 te0 aft) a
      else G.impl (assumeOp2 env0 sic lc cmp0 te0 aft) a

    (** val assume : ZAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp **)

    let assume c a =
      let ti = c.ZAtomicCond.right in
      let te,aft = ZTerm.affineDecompose ti in
      (match te with
       | ZTerm.Cte c0 ->
         (match c0 with
          | Z0 -> affAssume c.ZAtomicCond.cmpOp aft a
          | _ -> assumeOp false c.ZAtomicCond.cmpOp te aft ti a)
       | _ -> assumeOp false c.ZAtomicCond.cmpOp te aft ti a)

    (** val getItvMode :
        mode -> ZTerm.term -> BasicD.t -> ZNItv.t Core.Base.imp **)

    let getItvMode mo te a =
      intervalize (fun _ -> ZNItv.top) false mo (ZTerm.annotAFFINE te) a
   end

  module Dom =
   struct
    type t = AtomicD.t

    (** val isIncl : t -> t -> bool Core.Base.imp **)

    let isIncl =
      AtomicD.isIncl

    (** val top : t **)

    let top =
      AtomicD.top

    (** val join : t -> t -> t Core.Base.imp **)

    let join =
      AtomicD.join

    (** val widen : t -> t -> t Core.Base.imp **)

    let widen =
      AtomicD.widen

    (** val bottom : t **)

    let bottom =
      AtomicD.bottom

    (** val isBottom : t -> bool Core.Base.imp **)

    let isBottom =
      AtomicD.isBottom

    (** val project : t -> PVar.t -> t Core.Base.imp **)

    let project =
      AtomicD.project

    (** val skipBottom :
        AtomicD.t -> AtomicD.t Core.Base.imp -> AtomicD.t Core.Base.imp **)

    let skipBottom a k =
      if AtomicD.isBottom a then a else k

    (** val assumeRec : ZCond.t -> AtomicD.t -> AtomicD.t Core.Base.imp **)

    let rec assumeRec c a =
      match c with
      | ZCond.Basic b -> if b then a else AtomicD.bottom
      | ZCond.Atom (cmp0, tl, tr) ->
        AtomicD.assume (ZAtomicCond.make tl cmp0 tr) a
      | ZCond.BinL (op, cl, cr) ->
        (match op with
         | AND ->
           let aux = assumeRec cl a in skipBottom aux (assumeRec cr aux)
         | OR -> AtomicD.join (assumeRec cl a) (assumeRec cr a))
      | ZCond.Not _ ->
        (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
          INTERN
          ('a'::('s'::('s'::('u'::('m'::('e'::(':'::('N'::('o'::('t'::[]))))))))))
          AtomicD.top

    (** val assume : ZCond.cond -> AtomicD.t -> AtomicD.t Core.Base.imp **)

    let assume c a =
      skipBottom a (assumeRec (ZCond.nnf c) a)

    module Naive =
     struct
      (** val coq_assert : ZCond.cond -> t -> bool Core.Base.imp **)

      let coq_assert c a =
        isBottom (assume (ZCond.Not c) a)
     end

    (** val assertRec : ZCond.t -> t -> bool Core.Base.imp **)

    let rec assertRec c a =
      match c with
      | ZCond.Basic b -> if b then true else isBottom a
      | ZCond.Atom (oc, tl, tr) ->
        (match oc with
         | Eq ->
           if Naive.coq_assert (ZCond.Atom (Le, tl, tr)) a
           then Naive.coq_assert (ZCond.Atom (Le, tr, tl)) a
           else false
         | _ -> Naive.coq_assert c a)
      | ZCond.BinL (op, cl, cr) ->
        (match op with
         | AND -> if assertRec cl a then assertRec cr a else false
         | OR -> Naive.coq_assert c a)
      | ZCond.Not _ -> Naive.coq_assert c a

    (** val coq_assert : ZCond.t -> t -> bool Core.Base.imp **)

    let coq_assert c a =
      assertRec (ZCond.nnf c) a
   end

  (** val encode : bool -> positive -> positive **)

  let encode b p =
    if b then Coq_xI p else Coq_xO p

  (** val decode : positive -> positive **)

  let decode = function
  | Coq_xI p0 -> p0
  | Coq_xO p0 -> p0
  | Coq_xH -> Coq_xH

  (** val encodeE : PositiveSet.t -> positive -> positive **)

  let encodeE r x =
    encode (PositiveSet.mem x r) x

  (** val encodeO : PositiveSet.t -> positive -> positive **)

  let encodeO r x =
    encode (negb (PositiveSet.mem x r)) x

  (** val decodeIn :
      PositiveSet.t -> ZNum.t Mem.t -> ZNum.t Mem.t -> positive -> ZNum.t **)

  let decodeIn r aux m = function
  | Coq_xI p0 -> if PositiveSet.mem p0 r then m p0 else aux (Coq_xI p0)
  | Coq_xO p0 -> if PositiveSet.mem p0 r then aux (Coq_xO p0) else m p0
  | Coq_xH -> aux Coq_xH

  (** val switch : PositiveSet.t -> positive -> PositiveSet.t **)

  let rec switch m i =
    match m with
    | PositiveSet.Leaf -> PositiveSet.add i PositiveSet.Leaf
    | PositiveSet.Node (l, o, r) ->
      (match i with
       | Coq_xI p -> PositiveSet.node l o (switch r p)
       | Coq_xO p -> PositiveSet.node (switch l p) o r
       | Coq_xH -> PositiveSet.node l (negb o) r)

  type assignDomain = { pol : Dom.t; renaming : PositiveSet.t }

  (** val pol : assignDomain -> Dom.t **)

  let pol a =
    a.pol

  (** val renaming : assignDomain -> PositiveSet.t **)

  let renaming a =
    a.renaming

  type t = assignDomain

  (** val top : t **)

  let top =
    { pol = Dom.top; renaming = PositiveSet.empty }

  (** val bottom : t **)

  let bottom =
    { pol = Dom.bottom; renaming = PositiveSet.empty }

  (** val isBottom : t -> bool Core.Base.imp **)

  let isBottom p =
    Dom.isBottom (pol p)

  (** val assume : ZCond.cond -> t -> t Core.Base.imp **)

  let assume c a =
    { pol = (Dom.assume (ZCond.map c (encodeE (renaming a))) (pol a));
      renaming = (renaming a) }

  (** val coq_assert : ZCond.cond -> t -> bool Core.Base.imp **)

  let coq_assert c a =
    Dom.coq_assert (ZCond.map c (encodeE (renaming a))) (pol a)

  (** val project : t -> PVar.t -> t Core.Base.imp **)

  let project a x =
    { pol = (Dom.project (pol a) (encodeE (renaming a) x)); renaming =
      (renaming a) }

  (** val guassignAux : positive -> ZCond.cond -> t -> t Core.Base.imp **)

  let guassignAux x c a =
    let a0 =
      (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
        DEBUG
        ('g'::('u'::('a'::('s'::('s'::('i'::('g'::('n'::(' '::('c'::('a'::('l'::('l'::('e'::('d'::[])))))))))))))))
        a
    in
    { pol = (Dom.project (Dom.assume c (pol a0)) (encodeE (renaming a0) x));
    renaming = (switch (renaming a0) x) }

  (** val guassign : PVar.t -> ZCond.cond -> t -> t Core.Base.imp **)

  let guassign x c a =
    let f = encodeE (renaming a) in
    let c1 = ZCond.xmap c f (Mem.assign x (encodeO (renaming a) x) f) in
    guassignAux x c1 a

  (** val assign : positive -> ZCond.Term.term -> t -> t Core.Base.imp **)

  let assign x te a =
    let c = ZCond.Atom (Eq, (ZCond.Term.Var (encodeO (renaming a) x)),
      (ZCond.Term.coq_Old (ZCond.Term.map te (encodeE (renaming a)))))
    in
    guassignAux x c a

  (** val nop : PVar.t -> assignDomain -> t Core.Base.imp **)

  let nop x a =
    { pol =
      (AtomicD.rename (encodeE (renaming a) x) (encodeO (renaming a) x)
        (pol a)); renaming = (switch (renaming a) x) }

  (** val switchAll : PositiveSet.t -> t -> t Core.Base.imp **)

  let switchAll r a =
    PositiveSet.fold nop r a

  (** val isIncl : t -> t -> bool Core.Base.imp **)

  let isIncl a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    Dom.isIncl (pol (switchAll r2 (switchAll r1 a1))) (pol a2)

  (** val join : t -> t -> t Core.Base.imp **)

  let join a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (Dom.join (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }

  (** val widen : t -> t -> t Core.Base.imp **)

  let widen a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (Dom.widen (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }

  (** val rename :
      positive -> positive -> assignDomain -> assignDomain Core.Base.imp **)

  let rename x y a =
    { pol =
      (AtomicD.rename (encodeE (renaming a) x) (encodeE (renaming a) y)
        (pol a)); renaming = (renaming a) }

  (** val getItvMode : mode -> ZCond.Term.t -> t -> ZNItv.t Core.Base.imp **)

  let getItvMode mo te a =
    AtomicD.getItvMode mo (ZCond.Term.map te (encodeE (renaming a))) (pol a)

  (** val pr : t -> char list **)

  let pr a =
    AtomicD.pr (pol a)

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f a =
    AtomicD.to_string (fun x -> f (decode x)) (pol a)

  type rep = AtomicD.rep

  (** val backend_rep :
      t -> (AtomicD.rep*((PVar.t -> positive)*(positive -> PVar.t))) option **)

  let backend_rep a =
    match AtomicD.backend_rep (pol a) with
    | Some p ->
      let r,p0 = p in
      let fa,fu = p0 in
      Some (r,((fun x -> encodeE (renaming a) (fa x)),(fun x ->
      fu (decode x))))
    | None -> None

  (** val meet : t -> t -> t Core.Base.imp **)

  let meet a1 a2 =
    let r1 = PositiveSet.diff (renaming a1) (renaming a2) in
    let r2 = PositiveSet.diff (renaming a2) (renaming a1) in
    { pol = (AtomicD.meet (pol (switchAll r1 a1)) (pol (switchAll r2 a2)));
    renaming = (PositiveSet.inter (renaming a1) (renaming a2)) }
 end
