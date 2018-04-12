open ASTerm
open BinNat
open BinNums
open CIndex
open ConsSet
open CstrC
open Datatypes
open Debugging
open FMapAVL
open LinTerm
open List0
open Nat0
open NumC
open PeanoNat
open QArith_base
open Qcanon
open Ring_polynom
open Ring_polynom_AddOnQ

type __ = Obj.t

module M = Make(NatIndex)

type squares = (coq_PExpr*coq_N) list

(** val compute_squares : squares -> coq_PExpr **)

let compute_squares s =
  fold_right (fun s0 res ->
    let p,n = s0 in PEmul (res, (PEpow (p, (N.mul (Npos (Coq_xO Coq_xH)) n)))))
    (PEc { coq_Qnum = (Zpos Coq_xH); coq_Qden = Coq_xH }) s

module MapPoly =
 struct
  type t = coq_PExpr M.t

  (** val indexi : nat -> nat -> nat list **)

  let indexi i n =
    map (fun j -> if Nat.eq_dec i j then S O else O) (seq O n)

  (** val init_rec : nat -> Cs.t -> Cstr.t -> t **)

  let rec init_rec i l d =
    match i with
    | O -> M.empty
    | S p ->
      M.add (indexi p (length l)) (Cstr.to_PExpr (Cs.geti p l d))
        (init_rec p l d)

  (** val init : Cs.t -> Cstr.t -> t **)

  let init l d =
    init_rec (length l) l d

  type ind_cons = { ind : NatIndex.t; cons : NatIndex.t list }

  (** val ind : ind_cons -> NatIndex.t **)

  let ind x = x.ind

  (** val cons : ind_cons -> NatIndex.t list **)

  let cons x = x.cons

  type buildOrder = ind_cons list

  (** val find_or_1 : NatIndex.t -> t -> coq_PExpr **)

  let find_or_1 i m =
    match M.find i m with
    | Some p -> p
    | None ->
      (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
        CERT
        ('f'::('i'::('n'::('d'::('_'::('o'::('r'::('_'::('1'::(':'::(' '::('e'::('l'::('e'::('m'::('e'::('n'::('t'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::(' '::('m'::('a'::('p'::[])))))))))))))))))))))))))))))))))))
        (PEc { coq_Qnum = (Zpos Coq_xH); coq_Qden = Coq_xH })

  (** val cons_rec : NatIndex.t list -> t -> coq_PExpr **)

  let cons_rec l m =
    fold_right (fun i p -> PEmul ((find_or_1 i m), p)) (PEc { coq_Qnum =
      (Zpos Coq_xH); coq_Qden = Coq_xH }) l

  (** val cons_map : buildOrder -> t -> t **)

  let cons_map c m =
    fold_right (fun ic map0 -> M.add ic.ind (cons_rec ic.cons map0) map0) m c

  (** val to_posQ : coq_Q -> coq_Q **)

  let to_posQ q =
    if coq_Qlt_le_dec { coq_Qnum = Z0; coq_Qden = Coq_xH } q
    then q
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('A'::(' '::('c'::('o'::('e'::('f'::('f'::('i'::('c'::('i'::('e'::('n'::('t'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('H'::('a'::('n'::('d'::('e'::('l'::('m'::('a'::('n'::(' '::('c'::('e'::('r'::('t'::('i'::('f'::('i'::('c'::('a'::('t'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('p'::('o'::('s'::('i'::('t'::('i'::('v'::('e'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
           { coq_Qnum = Z0; coq_Qden = Coq_xH }

  (** val to_pos1 : (coq_Q*'a1) -> coq_Q*'a1 **)

  let to_pos1 = function
  | q,y -> (to_posQ q),y

  (** val to_pos : (coq_Q*'a1) list -> (coq_Q*'a1) list **)

  let to_pos l =
    map to_pos1 l

  (** val compute_varBound : Cs.t -> QcIndex.t -> nat -> coq_PExpr **)

  let rec compute_varBound p vb i =
    match vb with
    | [] -> PEc { coq_Qnum = Z0; coq_Qden = Coq_xH }
    | qc::tl ->
      let cstr = Cstr.to_PExpr (Cs.geti i p Cs.default) in
      let qc' = to_posQ (this qc) in
      PEadd ((PEmul ((PEc qc'), cstr)), (compute_varBound p tl (add i (S O))))

  (** val compute_varBounds : Cs.t -> QcIndex.t list -> coq_PExpr **)

  let compute_varBounds p vbs =
    fold_right (fun vb res -> PEmul (res, (compute_varBound p vb O))) (PEc
      { coq_Qnum = (Zpos Coq_xH); coq_Qden = Coq_xH }) vbs

  type schweighofer = (NatIndex.t*squares)*QcIndex.t list

  (** val calculus :
      Cs.t -> t -> (coq_Q*((NatIndex.t*squares)*QcIndex.t list)) -> coq_Q
      Ring_polynom.coq_PExpr -> coq_Q Ring_polynom.coq_PExpr **)

  let calculus p m x p0 =
    let q,y = x in
    let y0,vbs = y in
    let i,s = y0 in
    let inter = PEmul ((compute_squares s), (PEmul (p0, (PEmul ((PEc q),
      (find_or_1 i m))))))
    in
    PEmul (inter, (compute_varBounds p vbs))

  (** val computeH :
      Cs.t -> (coq_Q*schweighofer) list -> buildOrder -> coq_PExpr **)

  let computeH p l c =
    let m = cons_map c (init p Cs.default) in
    fold_right (fun x p0 -> calculus p m x p0) (PEc { coq_Qnum = (Zpos
      Coq_xH); coq_Qden = Coq_xH }) (to_pos l)
 end

module Handelman_compute =
 struct
  type certif = { aff : QTerm.t; sch : (coq_Q*MapPoly.schweighofer) list;
                  bo : MapPoly.buildOrder }

  (** val aff : certif -> QTerm.t **)

  let aff x = x.aff

  (** val sch : certif -> (coq_Q*MapPoly.schweighofer) list **)

  let sch x = x.sch

  (** val bo : certif -> MapPoly.buildOrder **)

  let bo x = x.bo

  module QPom = QTerm2Pomial(QTerm)

  (** val witness : Cs.t -> certif -> QTerm.t -> coq_PExpr **)

  let witness p c g =
    PEadd ((QPom.toPExpr g), (MapPoly.computeH p c.sch c.bo))

  (** val one : QAffTerm.affTerm **)

  let one =
    { QAffTerm.lin = QAffTerm.Lin.nil; QAffTerm.cte = QNum.u }

  (** val eq_witness : Cs.t -> certif -> QTerm.t -> QAffTerm.t **)

  let eq_witness p c g =
    if coq_Peq coq_Qeq_bool
         (norm_aux { coq_Qnum = Z0; coq_Qden = Coq_xH } { coq_Qnum = (Zpos
           Coq_xH); coq_Qden = Coq_xH } coq_Qplus coq_Qmult coq_Qminus
           coq_Qopp coq_Qeq_bool (witness p c g))
         (norm_aux { coq_Qnum = Z0; coq_Qden = Coq_xH } { coq_Qnum = (Zpos
           Coq_xH); coq_Qden = Coq_xH } coq_Qplus coq_Qmult coq_Qminus
           coq_Qopp coq_Qeq_bool (QPom.toPExpr c.aff))
    then let te,aft = QTerm.affineDecompose c.aff in
         (match te with
          | QTerm.Cte c0 ->
            if let filtered_var = (this c0).coq_Qnum in
               (match filtered_var with
                | Z0 -> true
                | _ -> false)
            then aft
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   CERT
                   ('e'::('q'::('_'::('w'::('i'::('t'::('n'::('e'::('s'::('s'::(' '::(':'::(' '::('a'::('f'::('f'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('l'::('i'::('n'::('e'::('a'::('r'::[]))))))))))))))))))))))))))))))
                   one
          | _ ->
            (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              CERT
              ('e'::('q'::('_'::('w'::('i'::('t'::('n'::('e'::('s'::('s'::(' '::(':'::(' '::('a'::('f'::('f'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('l'::('i'::('n'::('e'::('a'::('r'::[]))))))))))))))))))))))))))))))
              one)
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('e'::('q'::('_'::('w'::('i'::('t'::('n'::('e'::('s'::('s'::(' '::(':'::(' '::('t'::('h'::('e'::(' '::('t'::('w'::('o'::(' '::('p'::('o'::('l'::('y'::('n'::('o'::('m'::('i'::('a'::('l'::('s'::(' '::('d'::('i'::('f'::('f'::('e'::('r' ::
           ('w' :: ':' :: CoqPr.exprPr' (witness p c g))
           @ ('a' :: ':' :: CoqPr.exprPr' (QPom.toPExpr c.aff))
           )))))))))))))))))))))))))))))))))))))))
           one
 end
