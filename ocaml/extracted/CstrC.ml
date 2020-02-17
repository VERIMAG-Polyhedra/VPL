open Debugging
open LinTerm
open NumC
open ProgVar
open Qcanon
open Ring_polynom
open Ring_polynom_AddOnQ
open String0

module Cstr =
 struct
  type tInd = { coefs : LinQ.t; typ : cmpT; cst : QNum.t }

  (** val coefs : tInd -> LinQ.t **)

  let coefs x = x.coefs

  (** val typ : tInd -> cmpT **)

  let typ x = x.typ

  (** val cst : tInd -> QNum.t **)

  let cst x = x.cst

  type t = tInd

  (** val cmpPr : cmpT -> char list **)

  let cmpPr = function
  | EqT -> '='::[]
  | LeT -> '<'::('='::[])
  | LtT -> '<'::[]

  (** val pr : t -> char list **)

  let pr c =
    append (LinQ.pr c.coefs)
      (append (' '::[])
        (append (cmpPr c.typ) (append (' '::[]) (QNum.pr c.cst))))

  (** val to_string : (PVar.t -> char list) -> t -> char list **)

  let to_string f c =
    append (LinQ.to_string f c.coefs)
      (append (' '::[])
        (append (cmpPr c.typ) (append (' '::[]) (QNum.pr c.cst))))

  (** val top : tInd **)

  let top =
    { coefs = LinQ.nil; typ = EqT; cst = QNum.z }

  (** val triv : cmpT -> QNum.t -> tInd **)

  let triv typ0 n =
    if match typ0 with
       | EqT -> coq_Qc_eq_dec QNum.z n
       | LeT -> if coq_Qclt_le_dec n QNum.z then false else true
       | LtT -> coq_Qclt_le_dec QNum.z n
    then { coefs = LinQ.nil; typ = typ0; cst = n }
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('t'::('r'::('i'::('v'::(':'::(' '::('u'::('n'::('s'::('a'::('t'::(' '::('a'::('r'::('g'::[])))))))))))))))
           top

  (** val to_le : t -> t **)

  let to_le c =
    { coefs = c.coefs; typ = LeT; cst = c.cst }

  (** val isContrad : t -> bool **)

  let isContrad c =
    if LinQ.isEq c.coefs LinQ.nil
    then (match c.typ with
          | EqT -> if coq_Qc_eq_dec QNum.z c.cst then false else true
          | LeT -> if coq_Qclt_le_dec c.cst QNum.z then true else false
          | LtT -> if coq_Qclt_le_dec QNum.z c.cst then false else true)
    else false

  (** val isEq : t -> t -> bool **)

  let isEq c1 c2 =
    if if coq_Qc_eq_dec c1.cst c2.cst then cmpT_eq c1.typ c2.typ else false
    then LinQ.isEq c1.coefs c2.coefs
    else false

  (** val cmpAdd : cmpT -> cmpT -> cmpT **)

  let cmpAdd t1 t2 =
    match t1 with
    | EqT -> t2
    | LeT -> (match t2 with
              | EqT -> LeT
              | x -> x)
    | LtT -> LtT

  (** val add : t -> t -> t **)

  let add c1 c2 =
    { coefs = (LinQ.add c1.coefs c2.coefs); typ = (cmpAdd c1.typ c2.typ);
      cst = (coq_Qcplus c1.cst c2.cst) }

  (** val mulSimpl : t -> QNum.t -> t **)

  let mulSimpl c n =
    { coefs = (LinQ.mul n c.coefs); typ = c.typ; cst = (coq_Qcmult n c.cst) }

  (** val mul : QNum.t -> t -> t **)

  let mul n c =
    match c.typ with
    | EqT -> mulSimpl c n
    | _ ->
      if coq_Qclt_le_dec QNum.z n
      then mulSimpl c n
      else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
             CERT ('m'::('u'::('l'::[]))) top

  (** val merge : tInd -> tInd -> t **)

  let merge c1 c2 =
    match c1.typ with
    | LeT ->
      (match c2.typ with
       | LeT ->
         if LinQ.isEq c2.coefs (LinQ.opp c1.coefs)
         then if coq_Qc_eq_dec c2.cst (coq_Qcopp c1.cst)
              then { coefs = c1.coefs; typ = EqT; cst = c1.cst }
              else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                     CERT ('m'::('e'::('r'::('g'::('e'::('_'::('1'::[])))))))
                     top
         else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                CERT ('m'::('e'::('r'::('g'::('e'::('_'::('2'::[]))))))) top
       | _ ->
         (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT ('m'::('e'::('r'::('g'::('e'::('_'::('3'::[]))))))) top)
    | _ ->
      (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
        CERT ('m'::('e'::('r'::('g'::('e'::('_'::('4'::[]))))))) top

  (** val isFree : PVar.t -> t -> bool **)

  let isFree x c =
    LinQ.isFree x c.coefs

  (** val rename : PVar.t -> PVar.t -> tInd -> t **)

  let rename x y c =
    { coefs = (LinQ.rename x y c.coefs); typ = c.typ; cst = c.cst }

  (** val upperToCstr : LinQ.t -> QNum.t -> t **)

  let upperToCstr v n =
    { coefs = v; typ = LtT; cst = n }

  (** val upperOrEqualsToCstr : LinQ.t -> QNum.t -> t **)

  let upperOrEqualsToCstr v n =
    { coefs = v; typ = LeT; cst = n }

  (** val lowerToCstr : LinQ.t -> QNum.t -> t **)

  let lowerToCstr v n =
    { coefs = (LinQ.opp v); typ = LtT; cst = (coq_Qcopp n) }

  (** val lowerOrEqualsToCstr : LinQ.t -> QNum.t -> t **)

  let lowerOrEqualsToCstr v n =
    { coefs = (LinQ.opp v); typ = LeT; cst = (coq_Qcopp n) }

  (** val to_PExpr : t -> coq_PExpr **)

  let to_PExpr cstr =
    PEsub ((PEc (QNum.to_Q cstr.cst)), (LinQ.to_PExpr cstr.coefs))
 end
