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

module ZTerm2Pomial =
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
 struct
  (** val toPExpr : ATerm.term -> Ring_polynom_AddOn.coq_PExpr **)

  let rec toPExpr = function
  | ATerm.Var x -> PEX x
  | ATerm.Cte c -> PEc c
  | ATerm.Add (tl, tr) -> PEadd ((toPExpr tl), (toPExpr tr))
  | ATerm.Opp te0 -> PEopp (toPExpr te0)
  | ATerm.Mul (tl, tr) -> PEmul ((toPExpr tl), (toPExpr tr))
  | ATerm.Annot (_, te0) -> toPExpr te0
 end

module ZPomialEquality =
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
 struct
  module M1 = ZTerm2Pomial(ATerm1)

  module M2 = ZTerm2Pomial(ATerm2)

  (** val pomial_eq : ATerm1.t -> ATerm2.t -> bool **)

  let pomial_eq te1 te2 =
    coq_Peq coq_Zeq_bool
      (norm_aux Z0 (Zpos Coq_xH) Z.add Z.mul Z.sub Z.opp coq_Zeq_bool
        (M1.toPExpr te1))
      (norm_aux Z0 (Zpos Coq_xH) Z.add Z.mul Z.sub Z.opp coq_Zeq_bool
        (M2.toPExpr te2))
 end

module QTerm2Pomial =
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
 struct
  (** val toPExpr : ATerm.term -> coq_PExpr **)

  let rec toPExpr = function
  | ATerm.Var x -> PEX x
  | ATerm.Cte c -> PEc (QNum.to_Q c)
  | ATerm.Add (tl, tr) -> PEadd ((toPExpr tl), (toPExpr tr))
  | ATerm.Opp te0 -> PEopp (toPExpr te0)
  | ATerm.Mul (tl, tr) -> PEmul ((toPExpr tl), (toPExpr tr))
  | ATerm.Annot (_, te0) -> toPExpr te0
 end

module TopLevelAnnot =
 struct
  type topLevelAnnot =
  | OLD
  | AFFINE
  | INTERV
  | STATIC
  | SKIP_ORACLE

  (** val topLevelAnnot_rect :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1 **)

  let topLevelAnnot_rect f f0 f1 f2 f3 = function
  | OLD -> f
  | AFFINE -> f0
  | INTERV -> f1
  | STATIC -> f2
  | SKIP_ORACLE -> f3

  (** val topLevelAnnot_rec :
      'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> topLevelAnnot -> 'a1 **)

  let topLevelAnnot_rec f f0 f1 f2 f3 = function
  | OLD -> f
  | AFFINE -> f0
  | INTERV -> f1
  | STATIC -> f2
  | SKIP_ORACLE -> f3

  type t = topLevelAnnot

  (** val pr : topLevelAnnot -> char list **)

  let pr = function
  | OLD -> 'O'::('L'::('D'::[]))
  | AFFINE -> 'A'::('F'::('F'::('I'::('N'::('E'::[])))))
  | INTERV -> 'I'::('N'::('T'::('E'::('R'::('V'::[])))))
  | STATIC -> 'S'::('T'::('A'::('T'::('I'::('C'::[])))))
  | SKIP_ORACLE ->
    'S'::('K'::('I'::('P'::('_'::('O'::('R'::('A'::('C'::('L'::('E'::[]))))))))))
 end

module ModalTerm =
 functor (N:NumSig) ->
 struct
  module Annot = TopLevelAnnot

  type term =
  | Var of PVar.t
  | Cte of N.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  (** val term_rect :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rect f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rect f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rect f f0 f1 f2 f3 f4 te)

  (** val term_rec :
      (PVar.t -> 'a1) -> (N.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1)
      -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rec f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rec f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rec f f0 f1 f2 f3 f4 te)

  type t = term

  (** val eval : term -> N.t Mem.t -> N.t **)

  let rec eval te m =
    match te with
    | Var x -> m x
    | Cte c -> c
    | Add (tl, tr) -> N.add (eval tl m) (eval tr m)
    | Opp te0 -> N.opp (eval te0 m)
    | Mul (tl, tr) -> N.mul (eval tl m) (eval tr m)
    | Annot (_, te0) -> eval te0 m

  (** val mdBound : term -> PVar.t -> PVar.t **)

  let rec mdBound te bound =
    match te with
    | Var x -> PVar.max bound x
    | Cte _ -> bound
    | Add (tl, tr) -> mdBound tl (mdBound tr bound)
    | Opp te0 -> mdBound te0 bound
    | Mul (tl, tr) -> mdBound tl (mdBound tr bound)
    | Annot (_, te0) -> mdBound te0 bound

  (** val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec fold_variables te f i =
    match te with
    | Var x -> f x i
    | Cte _ -> i
    | Add (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Opp te0 -> fold_variables te0 f i
    | Mul (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Annot (_, te0) -> fold_variables te0 f i

  (** val map : term -> PVar.t Mem.t -> term **)

  let rec map te f =
    match te with
    | Var x -> Var (f x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((map tl f), (map tr f))
    | Opp te0 -> Opp (map te0 f)
    | Mul (te0, tr) -> Mul ((map te0 f), (map tr f))
    | Annot (a, te0) -> Annot (a, (map te0 f))

  (** val pseudoIsZero : term -> bool **)

  let pseudoIsZero = function
  | Cte c -> if N.isZero c then true else false
  | _ -> false

  (** val smartScalAdd1 : N.t -> term -> term **)

  let smartScalAdd1 c te =
    if N.isZero c then te else Add ((Cte c), te)

  (** val smartScalAdd : N.t -> term -> term **)

  let smartScalAdd c te = match te with
  | Cte c' -> Cte (N.add c c')
  | _ -> smartScalAdd1 c te

  (** val smartAdd : term -> term -> term **)

  let smartAdd te1 te2 =
    match te1 with
    | Cte c -> smartScalAdd c te2
    | _ -> (match te2 with
            | Cte c -> smartScalAdd1 c te1
            | _ -> Add (te1, te2))

  (** val smartOpp : term -> term **)

  let smartOpp te = match te with
  | Cte c -> Cte (N.opp c)
  | _ -> Opp te

  (** val smartScalMul1 : N.t -> term -> term **)

  let smartScalMul1 c te =
    match N.mulDiscr c with
    | IsZero -> Cte N.z
    | IsUnit -> te
    | IsOppUnit -> Opp te
    | Other -> Mul (te, (Cte c))

  (** val smartScalMul : N.t -> term -> term **)

  let smartScalMul c te = match te with
  | Cte c' -> Cte (N.mul c c')
  | _ -> smartScalMul1 c te

  (** val smartMul : term -> term -> term **)

  let smartMul te1 te2 =
    match te1 with
    | Cte c -> smartScalMul c te2
    | _ -> (match te2 with
            | Cte c -> smartScalMul1 c te1
            | _ -> Mul (te1, te2))

  (** val smartAnnot : Annot.topLevelAnnot -> term -> term **)

  let smartAnnot a te = match te with
  | Cte _ -> te
  | _ -> Annot (a, te)

  (** val import_acc : (PVar.t*N.t) list -> term -> term **)

  let rec import_acc l acc =
    match l with
    | [] -> acc
    | p::l0 ->
      let x,c = p in import_acc l0 (Add (acc, (smartScalMul1 c (Var x))))

  (** val import : (PVar.t*N.t) list -> term **)

  let import = function
  | [] -> Cte N.z
  | p::l0 -> let x,c = p in import_acc l0 (smartScalMul1 c (Var x))

  (** val coq_Old : term -> term **)

  let coq_Old te =
    Annot (TopLevelAnnot.OLD, te)

  (** val xeval : term -> N.t Mem.t -> N.t Mem.t -> N.t **)

  let rec xeval te old new0 =
    match te with
    | Var x -> new0 x
    | Cte c -> c
    | Add (tl, tr) -> N.add (xeval tl old new0) (xeval tr old new0)
    | Opp te0 -> N.opp (xeval te0 old new0)
    | Mul (tl, tr) -> N.mul (xeval tl old new0) (xeval tr old new0)
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> eval te0 old
       | _ -> xeval te0 old new0)

  (** val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term **)

  let rec xmap te old new0 =
    match te with
    | Var x -> Var (new0 x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((xmap tl old new0), (xmap tr old new0))
    | Opp te0 -> Opp (xmap te0 old new0)
    | Mul (tl, tr) -> Mul ((xmap tl old new0), (xmap tr old new0))
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> coq_Old (map te0 old)
       | _ -> Annot (a, (xmap te0 old new0)))

  (** val isCte : term -> bool **)

  let rec isCte = function
  | Cte _ -> true
  | Annot (_, te0) -> isCte te0
  | _ -> false

  (** val annotAFFINEx : term -> term **)

  let annotAFFINEx te = match te with
  | Annot (a, _) ->
    (match a with
     | Annot.AFFINE -> te
     | _ -> Annot (TopLevelAnnot.AFFINE, te))
  | _ -> Annot (TopLevelAnnot.AFFINE, te)

  (** val annotAFFINE_rec : term -> term option **)

  let rec annotAFFINE_rec = function
  | Add (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add (tl', tr'))
        | None -> Some (Add (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add ((annotAFFINEx tl), tr'))
        | None -> None))
  | Opp t0 ->
    (match annotAFFINE_rec t0 with
     | Some t0' -> Some (Opp t0')
     | None -> None)
  | Mul (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tl', tr'))
        | None -> Some (Mul (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tr', (annotAFFINEx tl)))
        | None ->
          if if isCte tl then true else isCte tr
          then None
          else Some (Mul ((annotAFFINEx tl), (annotAFFINEx tr)))))
  | Annot (a, t0) ->
    (match a with
     | Annot.AFFINE -> None
     | _ ->
       (match annotAFFINE_rec t0 with
        | Some t0' -> Some (Annot (a, t0'))
        | None -> None))
  | _ -> None

  (** val annotAFFINE : term -> term **)

  let annotAFFINE te =
    match annotAFFINE_rec te with
    | Some te' -> te'
    | None -> annotAFFINEx te

  (** val matchCte : term -> N.t option **)

  let rec matchCte = function
  | Cte c -> Some c
  | Annot (_, te0) -> matchCte te0
  | _ -> None

  (** val pr : term -> char list **)

  let rec pr = function
  | Var x -> PVar.pr x
  | Cte c -> N.pr c
  | Add (tl, tr) -> append (pr tl) (append ('+'::[]) (pr tr))
  | Opp te0 -> append ('-'::('('::[])) (append (pr te0) (')'::[]))
  | Mul (tl, tr) ->
    append ('('::[])
      (append (pr tl)
        (append (')'::('*'::('('::[]))) (append (pr tr) (')'::[]))))
  | Annot (a, te0) ->
    append (Annot.pr a) (append ('('::[]) (append (pr te0) (')'::[])))
 end

module BasicQTerm = ModalTerm(QNum)

module QTerm =
 struct
  module Annot = TopLevelAnnot

  type term = BasicQTerm.term =
  | Var of PVar.t
  | Cte of QNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  (** val term_rect :
      (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rect f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rect f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rect f f0 f1 f2 f3 f4 te)

  (** val term_rec :
      (PVar.t -> 'a1) -> (QNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rec f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rec f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rec f f0 f1 f2 f3 f4 te)

  type t = term

  (** val eval : term -> QNum.t Mem.t -> QNum.t **)

  let rec eval te m =
    match te with
    | Var x -> m x
    | Cte c -> c
    | Add (tl, tr) -> coq_Qcplus (eval tl m) (eval tr m)
    | Opp te0 -> coq_Qcopp (eval te0 m)
    | Mul (tl, tr) -> coq_Qcmult (eval tl m) (eval tr m)
    | Annot (_, te0) -> eval te0 m

  (** val mdBound : term -> PVar.t -> PVar.t **)

  let rec mdBound te bound =
    match te with
    | Var x -> PVar.max bound x
    | Cte _ -> bound
    | Add (tl, tr) -> mdBound tl (mdBound tr bound)
    | Opp te0 -> mdBound te0 bound
    | Mul (tl, tr) -> mdBound tl (mdBound tr bound)
    | Annot (_, te0) -> mdBound te0 bound

  (** val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec fold_variables te f i =
    match te with
    | Var x -> f x i
    | Cte _ -> i
    | Add (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Opp te0 -> fold_variables te0 f i
    | Mul (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Annot (_, te0) -> fold_variables te0 f i

  (** val map : term -> PVar.t Mem.t -> term **)

  let rec map te f =
    match te with
    | Var x -> Var (f x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((map tl f), (map tr f))
    | Opp te0 -> Opp (map te0 f)
    | Mul (te0, tr) -> Mul ((map te0 f), (map tr f))
    | Annot (a, te0) -> Annot (a, (map te0 f))

  (** val pseudoIsZero : term -> bool **)

  let pseudoIsZero = function
  | Cte c ->
    if let filtered_var = (this c).coq_Qnum in
       (match filtered_var with
        | Z0 -> true
        | _ -> false)
    then true
    else false
  | _ -> false

  (** val smartScalAdd1 : QNum.t -> term -> term **)

  let smartScalAdd1 c te =
    if let filtered_var = (this c).coq_Qnum in
       (match filtered_var with
        | Z0 -> true
        | _ -> false)
    then te
    else Add ((Cte c), te)

  (** val smartScalAdd : QNum.t -> term -> term **)

  let smartScalAdd c te = match te with
  | Cte c' -> Cte (coq_Qcplus c c')
  | _ ->
    if let filtered_var = (this c).coq_Qnum in
       (match filtered_var with
        | Z0 -> true
        | _ -> false)
    then te
    else Add ((Cte c), te)

  (** val smartAdd : term -> term -> term **)

  let smartAdd te1 te2 =
    match te1 with
    | Cte c -> smartScalAdd c te2
    | _ ->
      (match te2 with
       | Cte c ->
         if let filtered_var = (this c).coq_Qnum in
            (match filtered_var with
             | Z0 -> true
             | _ -> false)
         then te1
         else Add ((Cte c), te1)
       | _ -> Add (te1, te2))

  (** val smartOpp : term -> term **)

  let smartOpp te = match te with
  | Cte c -> Cte (coq_Qcopp c)
  | _ -> Opp te

  (** val smartScalMul1 : QNum.t -> term -> term **)

  let smartScalMul1 c te =
    match (this c).coq_Qden with
    | Coq_xH ->
      (match (this c).coq_Qnum with
       | Z0 -> Cte QNum.z
       | Zpos p -> (match p with
                    | Coq_xH -> te
                    | _ -> Mul (te, (Cte c)))
       | Zneg p -> (match p with
                    | Coq_xH -> Opp te
                    | _ -> Mul (te, (Cte c))))
    | _ -> Mul (te, (Cte c))

  (** val smartScalMul : QNum.t -> term -> term **)

  let smartScalMul c te = match te with
  | Cte c' -> Cte (coq_Qcmult c c')
  | _ -> smartScalMul1 c te

  (** val smartMul : term -> term -> term **)

  let smartMul te1 te2 =
    match te1 with
    | Cte c ->
      (match te2 with
       | Cte c' -> Cte (coq_Qcmult c c')
       | _ -> smartScalMul1 c te2)
    | _ -> (match te2 with
            | Cte c -> smartScalMul1 c te1
            | _ -> Mul (te1, te2))

  (** val smartAnnot : Annot.topLevelAnnot -> term -> term **)

  let smartAnnot a te = match te with
  | Cte _ -> te
  | _ -> Annot (a, te)

  (** val import_acc : (PVar.t*QNum.t) list -> term -> term **)

  let rec import_acc l acc =
    match l with
    | [] -> acc
    | p::l0 ->
      let x,c = p in import_acc l0 (Add (acc, (smartScalMul1 c (Var x))))

  (** val import : (PVar.t*QNum.t) list -> term **)

  let import = function
  | [] -> Cte QNum.z
  | p::l0 -> let x,c = p in import_acc l0 (smartScalMul1 c (Var x))

  (** val coq_Old : term -> term **)

  let coq_Old te =
    Annot (TopLevelAnnot.OLD, te)

  (** val xeval : term -> QNum.t Mem.t -> QNum.t Mem.t -> QNum.t **)

  let rec xeval te old new0 =
    match te with
    | Var x -> new0 x
    | Cte c -> c
    | Add (tl, tr) -> coq_Qcplus (xeval tl old new0) (xeval tr old new0)
    | Opp te0 -> coq_Qcopp (xeval te0 old new0)
    | Mul (tl, tr) -> coq_Qcmult (xeval tl old new0) (xeval tr old new0)
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> eval te0 old
       | _ -> xeval te0 old new0)

  (** val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term **)

  let rec xmap te old new0 =
    match te with
    | Var x -> Var (new0 x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((xmap tl old new0), (xmap tr old new0))
    | Opp te0 -> Opp (xmap te0 old new0)
    | Mul (tl, tr) -> Mul ((xmap tl old new0), (xmap tr old new0))
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> coq_Old (map te0 old)
       | _ -> Annot (a, (xmap te0 old new0)))

  (** val isCte : term -> bool **)

  let rec isCte = function
  | Cte _ -> true
  | Annot (_, te0) -> isCte te0
  | _ -> false

  (** val annotAFFINEx : term -> term **)

  let annotAFFINEx te = match te with
  | Annot (a, _) ->
    (match a with
     | Annot.AFFINE -> te
     | _ -> Annot (TopLevelAnnot.AFFINE, te))
  | _ -> Annot (TopLevelAnnot.AFFINE, te)

  (** val annotAFFINE_rec : term -> term option **)

  let rec annotAFFINE_rec = function
  | Add (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add (tl', tr'))
        | None -> Some (Add (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add ((annotAFFINEx tl), tr'))
        | None -> None))
  | Opp t0 ->
    (match annotAFFINE_rec t0 with
     | Some t0' -> Some (Opp t0')
     | None -> None)
  | Mul (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tl', tr'))
        | None -> Some (Mul (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tr', (annotAFFINEx tl)))
        | None ->
          if if isCte tl then true else isCte tr
          then None
          else Some (Mul ((annotAFFINEx tl), (annotAFFINEx tr)))))
  | Annot (a, t0) ->
    (match a with
     | Annot.AFFINE -> None
     | _ ->
       (match annotAFFINE_rec t0 with
        | Some t0' -> Some (Annot (a, t0'))
        | None -> None))
  | _ -> None

  (** val annotAFFINE : term -> term **)

  let annotAFFINE te =
    match annotAFFINE_rec te with
    | Some te' -> te'
    | None -> annotAFFINEx te

  (** val matchCte : term -> QNum.t option **)

  let rec matchCte = function
  | Cte c -> Some c
  | Annot (_, te0) -> matchCte te0
  | _ -> None

  (** val pr : term -> char list **)

  let rec pr = function
  | Var x -> PVar.pr x
  | Cte c -> QNum.pr c
  | Add (tl, tr) -> append (pr tl) (append ('+'::[]) (pr tr))
  | Opp te0 -> append ('-'::('('::[])) (append (pr te0) (')'::[]))
  | Mul (tl, tr) ->
    append ('('::[])
      (append (pr tl)
        (append (')'::('*'::('('::[]))) (append (pr tr) (')'::[]))))
  | Annot (a, te0) ->
    append (Annot.pr a) (append ('('::[]) (append (pr te0) (')'::[])))

  (** val fromLin : QAffTerm.Lin.t -> BasicQTerm.term **)

  let fromLin lt =
    BasicQTerm.import (QAffTerm.Lin.export lt)

  (** val fromAff : QAffTerm.affTerm -> BasicQTerm.term **)

  let fromAff aff =
    BasicQTerm.Add ((fromLin aff.QAffTerm.lin), (BasicQTerm.Cte
      aff.QAffTerm.cte))

  (** val affineDecompose : BasicQTerm.term -> BasicQTerm.term*QAffTerm.t **)

  let rec affineDecompose = function
  | BasicQTerm.Var x ->
    (BasicQTerm.Cte QNum.z),{ QAffTerm.lin = (QAffTerm.Lin.single x QNum.u);
      QAffTerm.cte = QNum.z }
  | BasicQTerm.Cte c ->
    (BasicQTerm.Cte QNum.z),{ QAffTerm.lin = QAffTerm.Lin.nil; QAffTerm.cte =
      c }
  | BasicQTerm.Add (tl, tr) ->
    let t1,aft1 = affineDecompose tl in
    let t2,aft2 = affineDecompose tr in
    (BasicQTerm.smartAdd t1 t2),(QAffTerm.add aft1 aft2)
  | BasicQTerm.Opp t0 ->
    let t1,aft = affineDecompose t0 in
    (BasicQTerm.smartOpp t1),(QAffTerm.opp aft)
  | BasicQTerm.Mul (tl, tr) ->
    let t1,aft1 = affineDecompose tl in
    let t2,aft2 = affineDecompose tr in
    let p1 =
      if if BasicQTerm.pseudoIsZero t2
         then QAffTerm.Lin.isNil aft2.QAffTerm.lin
         else false
      then BasicQTerm.Cte QNum.z
      else BasicQTerm.smartAdd t1 (fromLin aft1.QAffTerm.lin)
    in
    let p2 =
      if if BasicQTerm.pseudoIsZero t1
         then QAffTerm.Lin.isNil aft1.QAffTerm.lin
         else false
      then BasicQTerm.Cte QNum.z
      else BasicQTerm.smartAdd t2 (fromLin aft2.QAffTerm.lin)
    in
    (BasicQTerm.smartAdd (BasicQTerm.smartMul p1 p2)
      (BasicQTerm.smartAdd (BasicQTerm.smartScalMul aft1.QAffTerm.cte t2)
        (BasicQTerm.smartScalMul aft2.QAffTerm.cte t1))),(QAffTerm.add
                                                           (QAffTerm.mul
                                                             aft1.QAffTerm.cte
                                                             aft2)
                                                           { QAffTerm.lin =
                                                           (QAffTerm.Lin.mul
                                                             aft2.QAffTerm.cte
                                                             aft1.QAffTerm.lin);
                                                           QAffTerm.cte =
                                                           QNum.z })
  | BasicQTerm.Annot (a, t0) ->
    let t1,aft = affineDecompose t0 in (BasicQTerm.smartAnnot a t1),aft
 end

module BasicZTerm = ModalTerm(ZNum)

module ZTerm =
 struct
  module Annot = TopLevelAnnot

  type term = BasicZTerm.term =
  | Var of PVar.t
  | Cte of ZNum.t
  | Add of term * term
  | Opp of term
  | Mul of term * term
  | Annot of Annot.topLevelAnnot * term

  (** val term_rect :
      (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rect f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rect f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rect f f0 f1 f2 f3 f4 tl) tr (term_rect f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rect f f0 f1 f2 f3 f4 te)

  (** val term_rec :
      (PVar.t -> 'a1) -> (ZNum.t -> 'a1) -> (term -> 'a1 -> term -> 'a1 ->
      'a1) -> (term -> 'a1 -> 'a1) -> (term -> 'a1 -> term -> 'a1 -> 'a1) ->
      (Annot.topLevelAnnot -> term -> 'a1 -> 'a1) -> term -> 'a1 **)

  let rec term_rec f f0 f1 f2 f3 f4 = function
  | Var x -> f x
  | Cte c -> f0 c
  | Add (tl, tr) ->
    f1 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Opp te -> f2 te (term_rec f f0 f1 f2 f3 f4 te)
  | Mul (tl, tr) ->
    f3 tl (term_rec f f0 f1 f2 f3 f4 tl) tr (term_rec f f0 f1 f2 f3 f4 tr)
  | Annot (a, te) -> f4 a te (term_rec f f0 f1 f2 f3 f4 te)

  type t = term

  (** val eval : term -> ZNum.t Mem.t -> ZNum.t **)

  let rec eval te m =
    match te with
    | Var x -> m x
    | Cte c -> c
    | Add (tl, tr) -> Z.add (eval tl m) (eval tr m)
    | Opp te0 -> Z.opp (eval te0 m)
    | Mul (tl, tr) -> Z.mul (eval tl m) (eval tr m)
    | Annot (_, te0) -> eval te0 m

  (** val mdBound : term -> PVar.t -> PVar.t **)

  let rec mdBound te bound =
    match te with
    | Var x -> PVar.max bound x
    | Cte _ -> bound
    | Add (tl, tr) -> mdBound tl (mdBound tr bound)
    | Opp te0 -> mdBound te0 bound
    | Mul (tl, tr) -> mdBound tl (mdBound tr bound)
    | Annot (_, te0) -> mdBound te0 bound

  (** val fold_variables : term -> (PVar.t -> 'a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec fold_variables te f i =
    match te with
    | Var x -> f x i
    | Cte _ -> i
    | Add (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Opp te0 -> fold_variables te0 f i
    | Mul (tl, tr) -> fold_variables tl f (fold_variables tr f i)
    | Annot (_, te0) -> fold_variables te0 f i

  (** val map : term -> PVar.t Mem.t -> term **)

  let rec map te f =
    match te with
    | Var x -> Var (f x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((map tl f), (map tr f))
    | Opp te0 -> Opp (map te0 f)
    | Mul (te0, tr) -> Mul ((map te0 f), (map tr f))
    | Annot (a, te0) -> Annot (a, (map te0 f))

  (** val pseudoIsZero : term -> bool **)

  let pseudoIsZero = function
  | Cte c -> (match c with
              | Z0 -> true
              | _ -> false)
  | _ -> false

  (** val smartScalAdd1 : ZNum.t -> term -> term **)

  let smartScalAdd1 c te =
    match c with
    | Z0 -> te
    | _ -> Add ((Cte c), te)

  (** val smartScalAdd : ZNum.t -> term -> term **)

  let smartScalAdd c te = match te with
  | Cte c' -> Cte (Z.add c c')
  | _ -> (match c with
          | Z0 -> te
          | _ -> Add ((Cte c), te))

  (** val smartAdd : term -> term -> term **)

  let smartAdd te1 te2 =
    match te1 with
    | Cte c -> smartScalAdd c te2
    | _ ->
      (match te2 with
       | Cte c -> (match c with
                   | Z0 -> te1
                   | _ -> Add ((Cte c), te1))
       | _ -> Add (te1, te2))

  (** val smartOpp : term -> term **)

  let smartOpp te = match te with
  | Cte c -> Cte (Z.opp c)
  | _ -> Opp te

  (** val smartScalMul1 : ZNum.t -> term -> term **)

  let smartScalMul1 c te =
    match c with
    | Z0 -> Cte ZNum.z
    | Zpos p -> (match p with
                 | Coq_xH -> te
                 | _ -> Mul (te, (Cte c)))
    | Zneg p -> (match p with
                 | Coq_xH -> Opp te
                 | _ -> Mul (te, (Cte c)))

  (** val smartScalMul : ZNum.t -> term -> term **)

  let smartScalMul c te = match te with
  | Cte c' -> Cte (Z.mul c c')
  | _ -> smartScalMul1 c te

  (** val smartMul : term -> term -> term **)

  let smartMul te1 te2 =
    match te1 with
    | Cte c ->
      (match te2 with
       | Cte c' -> Cte (Z.mul c c')
       | _ -> smartScalMul1 c te2)
    | _ -> (match te2 with
            | Cte c -> smartScalMul1 c te1
            | _ -> Mul (te1, te2))

  (** val smartAnnot : Annot.topLevelAnnot -> term -> term **)

  let smartAnnot a te = match te with
  | Cte _ -> te
  | _ -> Annot (a, te)

  (** val import_acc : (PVar.t*ZNum.t) list -> term -> term **)

  let rec import_acc l acc =
    match l with
    | [] -> acc
    | p::l0 ->
      let x,c = p in import_acc l0 (Add (acc, (smartScalMul1 c (Var x))))

  (** val import : (PVar.t*ZNum.t) list -> term **)

  let import = function
  | [] -> Cte ZNum.z
  | p::l0 -> let x,c = p in import_acc l0 (smartScalMul1 c (Var x))

  (** val coq_Old : term -> term **)

  let coq_Old te =
    Annot (TopLevelAnnot.OLD, te)

  (** val xeval : term -> ZNum.t Mem.t -> ZNum.t Mem.t -> ZNum.t **)

  let rec xeval te old new0 =
    match te with
    | Var x -> new0 x
    | Cte c -> c
    | Add (tl, tr) -> Z.add (xeval tl old new0) (xeval tr old new0)
    | Opp te0 -> Z.opp (xeval te0 old new0)
    | Mul (tl, tr) -> Z.mul (xeval tl old new0) (xeval tr old new0)
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> eval te0 old
       | _ -> xeval te0 old new0)

  (** val xmap : term -> PVar.t Mem.t -> PVar.t Mem.t -> term **)

  let rec xmap te old new0 =
    match te with
    | Var x -> Var (new0 x)
    | Cte _ -> te
    | Add (tl, tr) -> Add ((xmap tl old new0), (xmap tr old new0))
    | Opp te0 -> Opp (xmap te0 old new0)
    | Mul (tl, tr) -> Mul ((xmap tl old new0), (xmap tr old new0))
    | Annot (a, te0) ->
      (match a with
       | Annot.OLD -> coq_Old (map te0 old)
       | _ -> Annot (a, (xmap te0 old new0)))

  (** val isCte : term -> bool **)

  let rec isCte = function
  | Cte _ -> true
  | Annot (_, te0) -> isCte te0
  | _ -> false

  (** val annotAFFINEx : term -> term **)

  let annotAFFINEx te = match te with
  | Annot (a, _) ->
    (match a with
     | Annot.AFFINE -> te
     | _ -> Annot (TopLevelAnnot.AFFINE, te))
  | _ -> Annot (TopLevelAnnot.AFFINE, te)

  (** val annotAFFINE_rec : term -> term option **)

  let rec annotAFFINE_rec = function
  | Add (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add (tl', tr'))
        | None -> Some (Add (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Add ((annotAFFINEx tl), tr'))
        | None -> None))
  | Opp t0 ->
    (match annotAFFINE_rec t0 with
     | Some t0' -> Some (Opp t0')
     | None -> None)
  | Mul (tl, tr) ->
    (match annotAFFINE_rec tl with
     | Some tl' ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tl', tr'))
        | None -> Some (Mul (tl', (annotAFFINEx tr))))
     | None ->
       (match annotAFFINE_rec tr with
        | Some tr' -> Some (Mul (tr', (annotAFFINEx tl)))
        | None ->
          if if isCte tl then true else isCte tr
          then None
          else Some (Mul ((annotAFFINEx tl), (annotAFFINEx tr)))))
  | Annot (a, t0) ->
    (match a with
     | Annot.AFFINE -> None
     | _ ->
       (match annotAFFINE_rec t0 with
        | Some t0' -> Some (Annot (a, t0'))
        | None -> None))
  | _ -> None

  (** val annotAFFINE : term -> term **)

  let annotAFFINE te =
    match annotAFFINE_rec te with
    | Some te' -> te'
    | None -> annotAFFINEx te

  (** val matchCte : term -> ZNum.t option **)

  let rec matchCte = function
  | Cte c -> Some c
  | Annot (_, te0) -> matchCte te0
  | _ -> None

  (** val pr : term -> char list **)

  let rec pr = function
  | Var x -> PVar.pr x
  | Cte c -> ZNum.pr c
  | Add (tl, tr) -> append (pr tl) (append ('+'::[]) (pr tr))
  | Opp te0 -> append ('-'::('('::[])) (append (pr te0) (')'::[]))
  | Mul (tl, tr) ->
    append ('('::[])
      (append (pr tl)
        (append (')'::('*'::('('::[]))) (append (pr tr) (')'::[]))))
  | Annot (a, te0) ->
    append (Annot.pr a) (append ('('::[]) (append (pr te0) (')'::[])))

  (** val fromLin : ZAffTerm.Lin.t -> BasicZTerm.term **)

  let fromLin lt =
    BasicZTerm.import (ZAffTerm.Lin.export lt)

  (** val fromAff : ZAffTerm.affTerm -> BasicZTerm.term **)

  let fromAff aff =
    BasicZTerm.Add ((fromLin aff.ZAffTerm.lin), (BasicZTerm.Cte
      aff.ZAffTerm.cte))

  (** val affineDecompose : BasicZTerm.term -> BasicZTerm.term*ZAffTerm.t **)

  let rec affineDecompose = function
  | BasicZTerm.Var x ->
    (BasicZTerm.Cte ZNum.z),{ ZAffTerm.lin = (ZAffTerm.Lin.single x ZNum.u);
      ZAffTerm.cte = ZNum.z }
  | BasicZTerm.Cte c ->
    (BasicZTerm.Cte ZNum.z),{ ZAffTerm.lin = ZAffTerm.Lin.nil; ZAffTerm.cte =
      c }
  | BasicZTerm.Add (tl, tr) ->
    let t1,aft1 = affineDecompose tl in
    let t2,aft2 = affineDecompose tr in
    (BasicZTerm.smartAdd t1 t2),(ZAffTerm.add aft1 aft2)
  | BasicZTerm.Opp t0 ->
    let t1,aft = affineDecompose t0 in
    (BasicZTerm.smartOpp t1),(ZAffTerm.opp aft)
  | BasicZTerm.Mul (tl, tr) ->
    let t1,aft1 = affineDecompose tl in
    let t2,aft2 = affineDecompose tr in
    let p1 =
      if if BasicZTerm.pseudoIsZero t2
         then ZAffTerm.Lin.isNil aft2.ZAffTerm.lin
         else false
      then BasicZTerm.Cte ZNum.z
      else BasicZTerm.smartAdd t1 (fromLin aft1.ZAffTerm.lin)
    in
    let p2 =
      if if BasicZTerm.pseudoIsZero t1
         then ZAffTerm.Lin.isNil aft1.ZAffTerm.lin
         else false
      then BasicZTerm.Cte ZNum.z
      else BasicZTerm.smartAdd t2 (fromLin aft2.ZAffTerm.lin)
    in
    (BasicZTerm.smartAdd (BasicZTerm.smartMul p1 p2)
      (BasicZTerm.smartAdd (BasicZTerm.smartScalMul aft1.ZAffTerm.cte t2)
        (BasicZTerm.smartScalMul aft2.ZAffTerm.cte t1))),(ZAffTerm.add
                                                           (ZAffTerm.mul
                                                             aft1.ZAffTerm.cte
                                                             aft2)
                                                           { ZAffTerm.lin =
                                                           (ZAffTerm.Lin.mul
                                                             aft2.ZAffTerm.cte
                                                             aft1.ZAffTerm.lin);
                                                           ZAffTerm.cte =
                                                           ZNum.z })
  | BasicZTerm.Annot (a, t0) ->
    let t1,aft = affineDecompose t0 in (BasicZTerm.smartAnnot a t1),aft
 end

type linearizeContext = { nonaffine : ZTerm.t; env : (PVar.t -> ZNItv.t);
                          affine : ZAffTerm.t; source : ZTerm.t; cmp : 
                          cmpG }
