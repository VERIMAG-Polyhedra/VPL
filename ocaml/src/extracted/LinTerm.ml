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

module PositiveMapVec =
 functor (N:NumSig) ->
 struct
  type t = N.t PositiveMap.t

  (** val absEval : (PVar.t*N.t) list -> N.t Mem.t -> N.t **)

  let absEval l m =
    fold_right (fun p sum -> N.add sum (N.mul (m (fst p)) (snd p))) N.z l

  (** val eval : t -> N.t Mem.t -> N.t **)

  let eval lt m =
    absEval (PositiveMap.elements lt) m

  type exportT = (PVar.t*N.t) list

  (** val export : t -> exportT **)

  let export lt =
    let rec xfoldi m v i =
      match m with
      | PositiveMap.Leaf -> v
      | PositiveMap.Node (l, o, r) ->
        (match o with
         | Some x ->
           xfoldi r
             ((i,x)::(xfoldi l v (FMapPositive.append i (Coq_xO Coq_xH))))
             (FMapPositive.append i (Coq_xI Coq_xH))
         | None ->
           xfoldi r (xfoldi l v (FMapPositive.append i (Coq_xO Coq_xH)))
             (FMapPositive.append i (Coq_xI Coq_xH)))
    in xfoldi lt [] Coq_xH

  (** val nil : t **)

  let nil =
    PositiveMap.Leaf

  (** val isNil : t -> bool **)

  let isNil = function
  | PositiveMap.Leaf -> true
  | PositiveMap.Node (_, _, _) -> false

  (** val single : PVar.t -> N.t -> t **)

  let single x n =
    if N.isZero n then nil else single x n

  (** val opp : t -> N.t PositiveMap.t **)

  let rec opp = function
  | PositiveMap.Leaf -> PositiveMap.Leaf
  | PositiveMap.Node (l, o, r) ->
    PositiveMap.Node ((opp l),
      (match o with
       | Some a -> Some (N.opp a)
       | None -> None), (opp r))

  (** val mul : N.t -> t -> t **)

  let mul n lt =
    match N.mulDiscr n with
    | IsZero -> nil
    | IsUnit -> lt
    | IsOppUnit -> opp lt
    | Other ->
      let rec map0 = function
      | PositiveMap.Leaf -> PositiveMap.Leaf
      | PositiveMap.Node (l, o, r) ->
        PositiveMap.Node ((map0 l),
          (match o with
           | Some a -> Some (N.mul n a)
           | None -> None), (map0 r))
      in map0 lt

  (** val coq_N_eqb : N.t -> N.t -> bool **)

  let coq_N_eqb x y =
    if N.eqDec x y then true else false

  (** val isEq : t -> t -> bool **)

  let isEq =
    equal coq_N_eqb

  (** val add : t -> t -> t **)

  let rec add m1 m2 =
    match m1 with
    | PositiveMap.Leaf -> m2
    | PositiveMap.Node (l1, o1, r1) ->
      (match m2 with
       | PositiveMap.Leaf -> m1
       | PositiveMap.Node (l2, o2, r2) ->
         node (add l1 l2)
           (nodeMerge (fun n1 n2 ->
             let r = N.add n1 n2 in if N.isZero r then None else Some r) o1
             o2) (add r1 r2))

  (** val isFree : PVar.t -> t -> bool **)

  let isFree x l =
    negb (PositiveMap.mem x l)

  (** val rename : PVar.t -> PVar.t -> t -> t **)

  let rename x y l =
    match PositiveMap.find x l with
    | Some c -> add (single y c) (PositiveMap.remove x l)
    | None -> l

  (** val fmtAux : char list list -> char list -> char list **)

  let rec fmtAux l s =
    match l with
    | [] -> s
    | prem::l' -> fmtAux l' (append prem s)

  (** val fmt : char list list -> char list **)

  let fmt = function
  | [] -> '0'::[]
  | prem::l' -> fmtAux l' prem

  (** val pairPr : (PVar.t*N.t) -> char list **)

  let pairPr p =
    append ('+'::(' '::[]))
      (append (PVar.pr (fst p))
        (append (' '::('*'::(' '::[]))) (N.pr (snd p))))

  (** val pr : t -> char list **)

  let pr lt =
    let l = PositiveMap.elements lt in fmt (rev (map pairPr l))

  (** val pair_to_string :
      (PVar.t -> char list) -> (PVar.t*N.t) -> char list **)

  let pair_to_string f p =
    append ('+'::(' '::[]))
      (append (f (fst p)) (append (' '::('*'::(' '::[]))) (N.pr (snd p))))

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f lt =
    let l = PositiveMap.elements lt in fmt (rev (map (pair_to_string f) l))
 end

module LinZ = PositiveMapVec(ZNum)

module LinQ =
 struct
  type t = QNum.t PositiveMap.t

  (** val absEval : (PVar.t*QNum.t) list -> QNum.t Mem.t -> QNum.t **)

  let absEval l m =
    fold_right (fun p sum -> coq_Qcplus sum (coq_Qcmult (m (fst p)) (snd p)))
      QNum.z l

  (** val eval : t -> QNum.t Mem.t -> QNum.t **)

  let eval lt m =
    absEval (PositiveMap.elements lt) m

  type exportT = (PVar.t*QNum.t) list

  (** val export : t -> exportT **)

  let export lt =
    let rec xfoldi m v i =
      match m with
      | PositiveMap.Leaf -> v
      | PositiveMap.Node (l, o, r) ->
        (match o with
         | Some x ->
           xfoldi r
             ((i,x)::(xfoldi l v (FMapPositive.append i (Coq_xO Coq_xH))))
             (FMapPositive.append i (Coq_xI Coq_xH))
         | None ->
           xfoldi r (xfoldi l v (FMapPositive.append i (Coq_xO Coq_xH)))
             (FMapPositive.append i (Coq_xI Coq_xH)))
    in xfoldi lt [] Coq_xH

  (** val nil : t **)

  let nil =
    PositiveMap.Leaf

  (** val isNil : t -> bool **)

  let isNil = function
  | PositiveMap.Leaf -> true
  | PositiveMap.Node (_, _, _) -> false

  (** val single : PVar.t -> QNum.t -> t **)

  let single x n =
    if let filtered_var = (this n).coq_Qnum in
       (match filtered_var with
        | Z0 -> true
        | _ -> false)
    then nil
    else single x n

  (** val opp : t -> QNum.t PositiveMap.t **)

  let rec opp = function
  | PositiveMap.Leaf -> PositiveMap.Leaf
  | PositiveMap.Node (l, o, r) ->
    PositiveMap.Node ((opp l),
      (match o with
       | Some a -> Some (coq_Qcopp a)
       | None -> None), (opp r))

  (** val mul : QNum.t -> t -> t **)

  let mul n lt =
    match (this n).coq_Qden with
    | Coq_xH ->
      (match (this n).coq_Qnum with
       | Z0 -> nil
       | Zpos p ->
         (match p with
          | Coq_xH -> lt
          | _ ->
            let rec map0 = function
            | PositiveMap.Leaf -> PositiveMap.Leaf
            | PositiveMap.Node (l, o, r) ->
              PositiveMap.Node ((map0 l),
                (match o with
                 | Some a -> Some (coq_Qcmult n a)
                 | None -> None), (map0 r))
            in map0 lt)
       | Zneg p ->
         (match p with
          | Coq_xH -> opp lt
          | _ ->
            let rec map0 = function
            | PositiveMap.Leaf -> PositiveMap.Leaf
            | PositiveMap.Node (l, o, r) ->
              PositiveMap.Node ((map0 l),
                (match o with
                 | Some a -> Some (coq_Qcmult n a)
                 | None -> None), (map0 r))
            in map0 lt))
    | _ ->
      let rec map0 = function
      | PositiveMap.Leaf -> PositiveMap.Leaf
      | PositiveMap.Node (l, o, r) ->
        PositiveMap.Node ((map0 l),
          (match o with
           | Some a -> Some (coq_Qcmult n a)
           | None -> None), (map0 r))
      in map0 lt

  (** val coq_N_eqb : QNum.t -> QNum.t -> bool **)

  let coq_N_eqb x y =
    if coq_Qc_eq_dec x y then true else false

  (** val isEq : t -> t -> bool **)

  let isEq =
    equal coq_N_eqb

  (** val add : t -> t -> t **)

  let rec add m1 m2 =
    match m1 with
    | PositiveMap.Leaf -> m2
    | PositiveMap.Node (l1, o1, r1) ->
      (match m2 with
       | PositiveMap.Leaf -> m1
       | PositiveMap.Node (l2, o2, r2) ->
         node (add l1 l2)
           (nodeMerge (fun n1 n2 ->
             let r = coq_Qcplus n1 n2 in
             if let filtered_var = (this r).coq_Qnum in
                (match filtered_var with
                 | Z0 -> true
                 | _ -> false)
             then None
             else Some r) o1 o2) (add r1 r2))

  (** val isFree : PVar.t -> t -> bool **)

  let isFree x l =
    negb (PositiveMap.mem x l)

  (** val rename : PVar.t -> PVar.t -> t -> t **)

  let rename x y l =
    match PositiveMap.find x l with
    | Some c -> add (single y c) (PositiveMap.remove x l)
    | None -> l

  (** val fmtAux : char list list -> char list -> char list **)

  let rec fmtAux l s =
    match l with
    | [] -> s
    | prem::l' -> fmtAux l' (append prem s)

  (** val fmt : char list list -> char list **)

  let fmt = function
  | [] -> '0'::[]
  | prem::l' -> fmtAux l' prem

  (** val pairPr : (PVar.t*QNum.t) -> char list **)

  let pairPr p =
    append ('+'::(' '::[]))
      (append (PVar.pr (fst p))
        (append (' '::('*'::(' '::[]))) (QNum.pr (snd p))))

  (** val pr : t -> char list **)

  let pr lt =
    let l = PositiveMap.elements lt in fmt (rev (map pairPr l))

  (** val pair_to_string :
      (PVar.t -> char list) -> (PVar.t*QNum.t) -> char list **)

  let pair_to_string f p =
    append ('+'::(' '::[]))
      (append (f (fst p)) (append (' '::('*'::(' '::[]))) (QNum.pr (snd p))))

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f lt =
    let l = PositiveMap.elements lt in fmt (rev (map (pair_to_string f) l))

  (** val exportT_to_PExpr : exportT -> coq_PExpr **)

  let rec exportT_to_PExpr = function
  | [] -> PEc { coq_Qnum = Z0; coq_Qden = Coq_xH }
  | p::tail ->
    let v,c = p in
    PEadd ((PEmul ((PEc (QNum.to_Q c)), (PEX v))), (exportT_to_PExpr tail))

  (** val to_PExpr : t -> coq_PExpr **)

  let to_PExpr lt =
    exportT_to_PExpr (export lt)

  (** val mem_compat : QNum.t Mem.t -> positive -> coq_Q **)

  let mem_compat m p =
    QNum.to_Q (m p)

  (** val import : exportT -> t **)

  let import l =
    fold_left (fun lt p -> add (single (fst p) (snd p)) lt) l nil

  (** val lift : LinZ.t -> t **)

  let rec lift = function
  | PositiveMap.Leaf -> PositiveMap.Leaf
  | PositiveMap.Node (l, o, r) ->
    PositiveMap.Node ((lift l),
      (match o with
       | Some a -> Some (inject_Z a)
       | None -> None), (lift r))
 end

module AffineTerm =
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
 struct
  type affTerm = { lin : L.t; cte : N.t }

  (** val lin : affTerm -> L.t **)

  let lin a =
    a.lin

  (** val cte : affTerm -> N.t **)

  let cte a =
    a.cte

  type t = affTerm

  (** val eval : affTerm -> N.t Mem.t -> N.t **)

  let eval aft m =
    N.add (L.eval (lin aft) m) (cte aft)

  (** val nil : affTerm **)

  let nil =
    { lin = L.nil; cte = N.z }

  (** val opp : affTerm -> affTerm **)

  let opp aft =
    { lin = (L.opp (lin aft)); cte = (N.opp (cte aft)) }

  (** val mul : N.t -> affTerm -> affTerm **)

  let mul c aft =
    { lin = (L.mul c (lin aft)); cte = (N.mul c (cte aft)) }

  (** val add : affTerm -> affTerm -> affTerm **)

  let add aft1 aft2 =
    { lin = (L.add (lin aft1) (lin aft2)); cte =
      (N.add (cte aft1) (cte aft2)) }

  (** val addc : N.t -> affTerm -> affTerm **)

  let addc c aft =
    { lin = (lin aft); cte = (N.add c (cte aft)) }

  (** val addx : PVar.t -> affTerm -> affTerm **)

  let addx x aft =
    { lin = (L.add (L.single x N.u) (lin aft)); cte = (cte aft) }

  (** val addnx : PVar.t -> affTerm -> affTerm **)

  let addnx x aft =
    { lin = (L.add (L.single x (N.opp N.u)) (lin aft)); cte = (cte aft) }

  (** val isZero : affTerm -> bool **)

  let isZero aft =
    if N.isZero (cte aft) then L.isNil (lin aft) else false
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

module ZAffTerm =
 struct
  module Lin = LinZ

  type affTerm = { lin : LinZ.t; cte : ZNum.t }

  (** val lin : affTerm -> LinZ.t **)

  let lin x = x.lin

  (** val cte : affTerm -> ZNum.t **)

  let cte x = x.cte

  type t = affTerm

  (** val eval : affTerm -> ZNum.t Mem.t -> ZNum.t **)

  let eval aft m =
    Z.add (LinZ.eval aft.lin m) aft.cte

  (** val nil : affTerm **)

  let nil =
    { lin = LinZ.nil; cte = ZNum.z }

  (** val opp : affTerm -> affTerm **)

  let opp aft =
    { lin = (LinZ.opp aft.lin); cte = (Z.opp aft.cte) }

  (** val mul : ZNum.t -> affTerm -> affTerm **)

  let mul c aft =
    { lin = (LinZ.mul c aft.lin); cte = (Z.mul c aft.cte) }

  (** val add : affTerm -> affTerm -> affTerm **)

  let add aft1 aft2 =
    { lin = (LinZ.add aft1.lin aft2.lin); cte = (Z.add aft1.cte aft2.cte) }

  (** val addc : ZNum.t -> affTerm -> affTerm **)

  let addc c aft =
    { lin = aft.lin; cte = (Z.add c aft.cte) }

  (** val addx : PVar.t -> affTerm -> affTerm **)

  let addx x aft =
    { lin = (LinZ.add (LinZ.single x ZNum.u) aft.lin); cte = aft.cte }

  (** val addnx : PVar.t -> affTerm -> affTerm **)

  let addnx x aft =
    { lin = (LinZ.add (LinZ.single x (Z.opp ZNum.u)) aft.lin); cte = aft.cte }

  (** val isZero : affTerm -> bool **)

  let isZero aft =
    match aft.cte with
    | Z0 -> LinZ.isNil aft.lin
    | _ -> false
 end

module QAffTerm =
 struct
  module Lin = LinQ

  type affTerm = { lin : LinQ.t; cte : QNum.t }

  (** val lin : affTerm -> LinQ.t **)

  let lin x = x.lin

  (** val cte : affTerm -> QNum.t **)

  let cte x = x.cte

  type t = affTerm

  (** val eval : affTerm -> QNum.t Mem.t -> QNum.t **)

  let eval aft m =
    coq_Qcplus (LinQ.eval aft.lin m) aft.cte

  (** val nil : affTerm **)

  let nil =
    { lin = LinQ.nil; cte = QNum.z }

  (** val opp : affTerm -> affTerm **)

  let opp aft =
    { lin = (LinQ.opp aft.lin); cte = (coq_Qcopp aft.cte) }

  (** val mul : QNum.t -> affTerm -> affTerm **)

  let mul c aft =
    { lin = (LinQ.mul c aft.lin); cte = (coq_Qcmult c aft.cte) }

  (** val add : affTerm -> affTerm -> affTerm **)

  let add aft1 aft2 =
    { lin = (LinQ.add aft1.lin aft2.lin); cte =
      (coq_Qcplus aft1.cte aft2.cte) }

  (** val addc : QNum.t -> affTerm -> affTerm **)

  let addc c aft =
    { lin = aft.lin; cte = (coq_Qcplus c aft.cte) }

  (** val addx : PVar.t -> affTerm -> affTerm **)

  let addx x aft =
    { lin = (LinQ.add (LinQ.single x QNum.u) aft.lin); cte = aft.cte }

  (** val addnx : PVar.t -> affTerm -> affTerm **)

  let addnx x aft =
    { lin = (LinQ.add (LinQ.single x (coq_Qcopp QNum.u)) aft.lin); cte =
      aft.cte }

  (** val isZero : affTerm -> bool **)

  let isZero aft =
    if let filtered_var = (this aft.cte).coq_Qnum in
       (match filtered_var with
        | Z0 -> true
        | _ -> false)
    then LinQ.isNil aft.lin
    else false

  (** val lift : ZAffTerm.t -> t **)

  let lift aft =
    { lin = (LinQ.lift aft.ZAffTerm.lin); cte = (inject_Z aft.ZAffTerm.cte) }
 end
