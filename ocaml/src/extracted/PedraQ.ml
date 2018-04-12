open ASAtomicCond
open ASCond
open ASTerm
open BinNums
open ConsSet
open CoqAddOn
open CstrC
open CstrLCF
open Datatypes
open Debugging
open DomainFunctors
open DomainInterfaces
open ImpureConfig
open Itv
open LinTerm
open LinearizeBackend
open List0
open MSetPositive
open Map_poly
open NumC
open PedraQBackend
open ProgVar
open QArith_base
open Qcanon
open String0

module BasicD =
 struct
  type polT = { cons : Cs.t; ml : t }

  (** val cons : polT -> Cs.t **)

  let cons x = x.cons

  (** val ml : polT -> t **)

  let ml x = x.ml

  (** val polPr : polT -> char list **)

  let polPr p =
    sprintf
      ('{'::('c'::('o'::('q'::(':'::('\\'::('n'::('%'::('s'::('\\'::('n'::('o'::('c'::('a'::('m'::('l'::(':'::('\\'::('n'::('%'::('s'::('}'::[]))))))))))))))))))))))
      ((Cs.pr p.cons)::((pr p.ml)::[]))

  (** val pol_to_string : (PVar.t -> char list) -> polT -> char list **)

  let pol_to_string f0 p =
    Cs.to_string f0 p.cons

  (** val wrap : polT -> Cs.cstr pedraCert **)

  let wrap p =
    { lcf = Cs.certCstrLCF; backend = p.ml; cert = (Cs.wrap p.cons) }

  (** val polTop : polT **)

  let polTop =
    { cons = []; ml = top }

  type t = polT option

  (** val pr : t -> char list **)

  let pr = function
  | Some p' ->
    append ('n'::('o'::('t'::('b'::('o'::('t'::(' '::[]))))))) (polPr p')
  | None -> 'b'::('o'::('t'::[]))

  (** val to_string : (PVar.t -> char list) -> polT option -> char list **)

  let to_string f0 = function
  | Some p' ->
    append ('n'::('o'::('t'::('b'::('o'::('t'::(' '::[])))))))
      (pol_to_string f0 p')
  | None -> 'b'::('o'::('t'::[]))

  (** val id : PVar.t -> PVar.t **)

  let id x =
    x

  type rep = PedraQBackend.t

  (** val backend_rep :
      polT option -> (PedraQBackend.t*((PVar.t -> PVar.t)*(PVar.t ->
      PVar.t))) option **)

  let backend_rep = function
  | Some p' -> Some (p'.ml,(id,id))
  | None -> None

  (** val top : t **)

  let top =
    Some polTop

  (** val my_assert : char list -> bool -> bool **)

  let my_assert mesg = function
  | true -> true
  | false ->
    (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
      CERT mesg false

  (** val bottom : t **)

  let bottom =
    None

  (** val isBottom : t -> bool Core.Base.imp **)

  let isBottom = function
  | Some pol0 ->
    (match isEmpty (wrap pol0) with
     | Some cert0 ->
       if Cstr.isContrad (Cs.rep cert0)
       then true
       else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              CERT
              ('i'::('s'::('B'::('o'::('t'::('t'::('o'::('m'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::[])))))))))))))))))))
              false
     | None -> false)
  | None -> true

  (** val isIncl : t -> t -> bool Core.Base.imp **)

  let isIncl p1 p2 =
    match p1 with
    | Some pol1 ->
      let wp1 = wrap pol1 in
      (match p2 with
       | Some pol2 ->
         (match isIncl (wp1,pol2.ml) with
          | Some p ->
            let b,cert0 = p in
            if b
            then if Cs.isContrad (Cs.unwrap cert0)
                 then true
                 else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                        CERT
                        ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('('::('2'::(')'::[])))))))))))))))))))))
                        false
            else if Cs.isEq (Cs.unwrap cert0) pol2.cons
                 then true
                 else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                        CERT
                        ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('d'::('o'::(' '::('n'::('o'::('t'::(' '::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))
                        false
          | None -> false)
       | None ->
         (match isEmpty wp1 with
          | Some cert0 ->
            if Cstr.isContrad (Cs.rep cert0)
            then true
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   CERT
                   ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('('::('1'::(')'::[])))))))))))))))))))))
                   false
          | None -> false))
    | None -> true

  (** val join : t -> t -> t Core.Base.imp **)

  let join p1 p2 =
    match p1 with
    | Some pol1 ->
      (match p2 with
       | Some pol2 ->
         let shadow,p = join ((wrap pol1),(wrap pol2)) in
         let cert1,cert2 = p in
         Some { cons = (Cs.join pol1.cons pol2.cons cert1 cert2); ml =
         shadow }
       | None -> p1)
    | None -> (match p2 with
               | Some _ -> p2
               | None -> None)

  (** val meet : t -> t -> t Core.Base.imp **)

  let meet p1 p2 =
    match p1 with
    | Some p1' ->
      (match p2 with
       | Some p2' ->
         let l1,l2 = Cs.wrap2 p1'.cons p2'.cons in
         let wp1 = { lcf = Cs.certCstrLCF; backend = p1'.ml; cert = l1 } in
         let opt,cert0 = meet (wp1,(p2'.ml,l2)) in
         let res = Cs.unwrap cert0 in
         (match opt with
          | Some p' -> Some { cons = res; ml = p' }
          | None ->
            if Cs.isContrad res
            then None
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   CERT
                   ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('m'::('e'::('e'::('t'::[])))))))))))
                   p1)
       | None -> None)
    | None -> None

  (** val widen : t -> t -> t Core.Base.imp **)

  let widen p1 p2 =
    match p1 with
    | Some pol1 ->
      (match p2 with
       | Some pol2 ->
         let cs,shadow = widen (pol1.ml,pol2.ml) in
         Some { cons = shadow; ml = cs }
       | None -> p1)
    | None -> (match p2 with
               | Some _ -> p2
               | None -> None)

  (** val project : t -> PVar.t -> t Core.Base.imp **)

  let project p x =
    match p with
    | Some p1 ->
      let shadow,cert0 = project ((wrap p1),x) in
      let res = Cs.unwrap cert0 in
      if Cs.isFree x res
      then Some { cons = res; ml = shadow }
      else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
             CERT
             (append
               ('p'::('r'::('o'::('j'::('e'::('c'::('t'::(':'::(' '::[])))))))))
               (append (PVar.pr x)
                 (append
                   (' '::('n'::('o'::('t'::(' '::('f'::('r'::('e'::('e'::(' '::('i'::('n'::(' '::[])))))))))))))
                   (Cs.pr res)))) top
    | None -> None
 end

module LinItvD =
 struct
  (** val buildLow : BasicD.polT -> LinQ.t -> Cs.cstr bndT -> QItv.bndT **)

  let buildLow _ v = function
  | Infty -> QItv.Infty
  | Open (n, c) ->
    let c0 = Cstr.lowerToCstr v n in
    if Cstr.isEq (Cs.rep c) c0
    then QItv.Open n
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('b'::('u'::('i'::('l'::('d'::('L'::('o'::('w'::('.'::('O'::('p'::('e'::('n'::[]))))))))))))))))))))
           QItv.Infty
  | Closed (n, c) ->
    let c0 = Cstr.lowerOrEqualsToCstr v n in
    if Cstr.isEq (Cs.rep c) c0
    then QItv.Closed n
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('b'::('u'::('i'::('l'::('d'::('L'::('o'::('w'::('.'::('C'::('l'::('o'::('s'::('e'::[])))))))))))))))))))))
           QItv.Infty

  (** val buildUp : BasicD.polT -> LinQ.t -> Cs.cstr bndT -> QItv.bndT **)

  let buildUp _ v = function
  | Infty -> QItv.Infty
  | Open (n, c) ->
    let c0 = Cstr.upperToCstr v n in
    if Cstr.isEq (Cs.rep c) c0
    then QItv.Open n
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('b'::('u'::('i'::('l'::('d'::('U'::('p'::('.'::('O'::('p'::('e'::('n'::[])))))))))))))))))))
           QItv.Infty
  | Closed (n, c) ->
    let c0 = Cstr.upperOrEqualsToCstr v n in
    if Cstr.isEq (Cs.rep c) c0
    then QItv.Closed n
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
           CERT
           ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('b'::('u'::('i'::('l'::('d'::('U'::('p'::('.'::('C'::('l'::('o'::('s'::('e'::[]))))))))))))))))))))
           QItv.Infty

  (** val getItv : BasicD.t -> LinQ.t -> QItv.t Core.Base.imp **)

  let getItv p v =
    match p with
    | Some pol0 ->
      let sItv = getItv ((BasicD.wrap pol0),v) in
      let low0 = buildLow pol0 v sItv.low in
      let up0 = buildUp pol0 v sItv.up in
      { QItv.lower = low0; QItv.upper = up0 }
    | None -> QItv.bot

  (** val getLowerBound : BasicD.t -> LinQ.t -> QItv.bndT Core.Base.imp **)

  let getLowerBound p v =
    match p with
    | Some pol0 -> buildLow pol0 v (getLowerBound ((BasicD.wrap pol0),v))
    | None ->
      (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
        CERT
        ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('I'::('t'::('v'::('D'::('.'::('g'::('e'::('t'::('L'::('o'::('w'::('e'::('r'::('B'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))
        QItv.Infty

  (** val getUpperBound : BasicD.t -> LinQ.t -> QItv.bndT Core.Base.imp **)

  let getUpperBound p v =
    match p with
    | Some pol0 -> buildUp pol0 v (getUpperBound ((BasicD.wrap pol0),v))
    | None ->
      (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
        CERT
        ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('I'::('t'::('v'::('D'::('.'::('g'::('e'::('t'::('U'::('p'::('p'::('e'::('r'::('B'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))
        QItv.Infty

  (** val getItvMode : mode -> LinQ.t -> BasicD.t -> QItv.t Core.Base.imp **)

  let getItvMode mo v p =
    match mo with
    | BOTH -> getItv p v
    | UP -> { QItv.lower = QItv.Infty; QItv.upper = (getUpperBound p v) }
    | LOW -> { QItv.lower = (getLowerBound p v); QItv.upper = QItv.Infty }
 end

module AffItvD =
 struct
  (** val getItvMode :
      mode -> QAffTerm.affTerm -> BasicD.t -> QItv.tInd Core.Base.imp **)

  let getItvMode mo aft a =
    QItv.shift
      (match mo with
       | BOTH -> LinItvD.getItv a aft.QAffTerm.lin
       | UP ->
         { QItv.lower = QItv.Infty; QItv.upper =
           (LinItvD.getUpperBound a aft.QAffTerm.lin) }
       | LOW ->
         { QItv.lower = (LinItvD.getLowerBound a aft.QAffTerm.lin);
           QItv.upper = QItv.Infty }) aft.QAffTerm.cte
 end

module ItvD =
 struct
  (** val getItvMode :
      mode -> BasicQTerm.term -> BasicD.t -> QItv.tInd Core.Base.imp **)

  let getItvMode mo te a =
    let te0,aft = QTerm.affineDecompose te in
    (match te0 with
     | QTerm.Cte c ->
       if let filtered_var = (this c).coq_Qnum in
          (match filtered_var with
           | Z0 -> true
           | _ -> false)
       then AffItvD.getItvMode mo aft a
       else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              INTERN
              ('g'::('e'::('t'::('I'::('t'::('v'::('M'::('o'::('d'::('e'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[])))))))))))))))))))))))))))
              QItv.top
     | _ ->
       (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
         INTERN
         ('g'::('e'::('t'::('I'::('t'::('v'::('M'::('o'::('d'::('e'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[])))))))))))))))))))))))))))
         QItv.top)

  (** val get_itv : BasicQTerm.term -> BasicD.t -> QItv.tInd Core.Base.imp **)

  let get_itv te a =
    let te0,aft = QTerm.affineDecompose te in
    (match te0 with
     | QTerm.Cte c ->
       if let filtered_var = (this c).coq_Qnum in
          (match filtered_var with
           | Z0 -> true
           | _ -> false)
       then AffItvD.getItvMode BOTH aft a
       else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              INTERN
              ('g'::('e'::('t'::('I'::('t'::('v'::('M'::('o'::('d'::('e'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[])))))))))))))))))))))))))))
              QItv.top
     | _ ->
       (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
         INTERN
         ('g'::('e'::('t'::('I'::('t'::('v'::('M'::('o'::('d'::('e'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[])))))))))))))))))))))))))))
         QItv.top)
 end

module CstrD =
 struct
  (** val assume : Cstr.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let assume c p = match p with
  | Some p1 ->
    let p2 =
      (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
        DEBUG
        (append
          ('a'::('s'::('s'::('u'::('m'::('e'::(' '::('i'::('n'::('p'::('u'::('t'::(':'::[])))))))))))))
          (append (BasicD.polPr p1)
            (append (' '::('/'::('\\'::(' '::[])))) (Cstr.pr c)))) p1
    in
    let l = Cs.wrap2 p2.BasicD.cons (c::[]) in
    let wp1 = { lcf = Cs.certCstrLCF; backend = p2.BasicD.ml; cert = (fst l) }
    in
    let opt,cert0 = add (wp1,(snd l)) in
    let res = Cs.unwrap cert0 in
    (match opt with
     | Some p' -> Some { BasicD.cons = res; BasicD.ml = p' }
     | None ->
       if Cs.isContrad res
       then None
       else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              CERT
              ('C'::('s'::('t'::('r'::('D'::('.'::('a'::('s'::('s'::('u'::('m'::('e'::[]))))))))))))
              p)
  | None -> None
 end

module Rename =
 struct
  (** val rename : PVar.t -> PVar.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let rename x y = function
  | Some p ->
    Some { BasicD.cons = (Cs.rename x y p.BasicD.cons); BasicD.ml =
      (rename ((x,y),p.BasicD.ml)) }
  | None -> None
 end

module QAtomicCondAssume =
 struct
  (** val affAssume :
      cmpG -> QAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let affAssume cmp aft a =
    match cmpG2T cmp with
    | Some cmpT -> CstrD.assume (QAtomicCond.toCstr cmpT aft) a
    | None ->
      BasicD.join (CstrD.assume (QAtomicCond.toCstr LtT aft) a)
        (CstrD.assume (QAtomicCond.toCstr LtT (QAffTerm.opp aft)) a)

  (** val applyHandelman_one :
      cmpG -> QTerm.t -> BasicD.polT -> BasicD.t -> Handelman_compute.certif
      -> BasicD.t Core.Base.imp **)

  let applyHandelman_one cmp qt pol0 a cert0 =
    let aff = Handelman_compute.eq_witness pol0.BasicD.cons cert0 qt in
    affAssume cmp aff a

  (** val f :
      cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif -> BasicD.t
      Core.Base.imp -> BasicD.t Core.Base.imp **)

  let f cmp qt pol0 cert0 a =
    applyHandelman_one cmp qt pol0 a cert0

  (** val applyHandelman :
      cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif list ->
      BasicD.t Core.Base.imp **)

  let applyHandelman cmp qt pol0 certs =
    fold_right (f cmp qt pol0) (Some pol0) certs

  (** val assume_eq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp **)

  let assume_eq qt pol0 =
    let opp0 = QTerm.Opp qt in
    BasicD.meet
      (applyHandelman Le qt pol0 (handelman_oracle pol0.BasicD.ml Le qt))
      (applyHandelman Le opp0 pol0 (handelman_oracle pol0.BasicD.ml Le opp0))

  (** val assume_neq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp **)

  let assume_neq qt pol0 =
    let opp0 = QTerm.Opp qt in
    BasicD.join
      (applyHandelman Lt qt pol0 (handelman_oracle pol0.BasicD.ml Lt qt))
      (applyHandelman Lt opp0 pol0 (handelman_oracle pol0.BasicD.ml Lt opp0))

  (** val assume : QAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let assume c a =
    let ti = c.QAtomicCond.right in
    let te,aft = QTerm.affineDecompose ti in
    (match te with
     | QTerm.Cte c0 ->
       if let filtered_var = (this c0).coq_Qnum in
          (match filtered_var with
           | Z0 -> true
           | _ -> false)
       then affAssume c.QAtomicCond.cmpOp aft a
       else (match a with
             | Some pol0 ->
               (match c.QAtomicCond.cmpOp with
                | Eq -> assume_eq ti pol0
                | Neq -> assume_neq ti pol0
                | _ ->
                  applyHandelman c.QAtomicCond.cmpOp ti pol0
                    (handelman_oracle pol0.BasicD.ml c.QAtomicCond.cmpOp ti))
             | None -> None)
     | _ ->
       (match a with
        | Some pol0 ->
          (match c.QAtomicCond.cmpOp with
           | Eq -> assume_eq ti pol0
           | Neq -> assume_neq ti pol0
           | _ ->
             applyHandelman c.QAtomicCond.cmpOp ti pol0
               (handelman_oracle pol0.BasicD.ml c.QAtomicCond.cmpOp ti))
        | None -> None))
 end

module AtomicD =
 struct
  type polT = BasicD.polT = { cons : Cs.t; ml : t }

  (** val cons : polT -> Cs.t **)

  let cons x = x.cons

  (** val ml : polT -> t **)

  let ml x = x.ml

  (** val polPr : polT -> char list **)

  let polPr p =
    sprintf
      ('{'::('c'::('o'::('q'::(':'::('\\'::('n'::('%'::('s'::('\\'::('n'::('o'::('c'::('a'::('m'::('l'::(':'::('\\'::('n'::('%'::('s'::('}'::[]))))))))))))))))))))))
      ((Cs.pr p.cons)::((pr p.ml)::[]))

  (** val pol_to_string : (PVar.t -> char list) -> polT -> char list **)

  let pol_to_string f0 p =
    Cs.to_string f0 p.cons

  (** val wrap : polT -> Cs.cstr pedraCert **)

  let wrap p =
    { lcf = Cs.certCstrLCF; backend = p.ml; cert = (Cs.wrap p.cons) }

  (** val polTop : polT **)

  let polTop =
    { cons = []; ml = top }

  type t = polT option

  (** val pr : t -> char list **)

  let pr = function
  | Some p' ->
    append ('n'::('o'::('t'::('b'::('o'::('t'::(' '::[]))))))) (polPr p')
  | None -> 'b'::('o'::('t'::[]))

  (** val to_string : (PVar.t -> char list) -> polT option -> char list **)

  let to_string f0 = function
  | Some p' ->
    append ('n'::('o'::('t'::('b'::('o'::('t'::(' '::[])))))))
      (pol_to_string f0 p')
  | None -> 'b'::('o'::('t'::[]))

  (** val id : PVar.t -> PVar.t **)

  let id x =
    x

  type rep = PedraQBackend.t

  (** val backend_rep :
      polT option -> (PedraQBackend.t*((PVar.t -> PVar.t)*(PVar.t ->
      PVar.t))) option **)

  let backend_rep = function
  | Some p' -> Some (p'.ml,(id,id))
  | None -> None

  (** val top : t **)

  let top =
    Some polTop

  (** val my_assert : char list -> bool -> bool **)

  let my_assert mesg = function
  | true -> true
  | false ->
    (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
      CERT mesg false

  (** val bottom : t **)

  let bottom =
    None

  (** val isBottom : t -> bool Core.Base.imp **)

  let isBottom = function
  | Some pol0 ->
    (match isEmpty (wrap pol0) with
     | Some cert0 ->
       if Cstr.isContrad (Cs.rep cert0)
       then true
       else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
              CERT
              ('i'::('s'::('B'::('o'::('t'::('t'::('o'::('m'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::[])))))))))))))))))))
              false
     | None -> false)
  | None -> true

  (** val isIncl : t -> t -> bool Core.Base.imp **)

  let isIncl p1 p2 =
    match p1 with
    | Some pol1 ->
      let wp1 = wrap pol1 in
      (match p2 with
       | Some pol2 ->
         (match isIncl (wp1,pol2.ml) with
          | Some p ->
            let b,cert0 = p in
            if b
            then if Cs.isContrad (Cs.unwrap cert0)
                 then true
                 else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                        CERT
                        ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('('::('2'::(')'::[])))))))))))))))))))))
                        false
            else if Cs.isEq (Cs.unwrap cert0) pol2.cons
                 then true
                 else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                        CERT
                        ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('d'::('o'::(' '::('n'::('o'::('t'::(' '::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))
                        false
          | None -> false)
       | None ->
         (match isEmpty wp1 with
          | Some cert0 ->
            if Cstr.isContrad (Cs.rep cert0)
            then true
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   CERT
                   ('i'::('s'::('I'::('n'::('c'::('l'::(':'::(' '::('n'::('o'::('t'::(' '::('e'::('m'::('p'::('t'::('y'::(' '::('('::('1'::(')'::[])))))))))))))))))))))
                   false
          | None -> false))
    | None -> true

  (** val join : t -> t -> t Core.Base.imp **)

  let join p1 p2 =
    match p1 with
    | Some pol1 ->
      (match p2 with
       | Some pol2 ->
         let shadow,p = join ((wrap pol1),(wrap pol2)) in
         let cert1,cert2 = p in
         Some { cons = (Cs.join pol1.cons pol2.cons cert1 cert2); ml =
         shadow }
       | None -> p1)
    | None -> (match p2 with
               | Some _ -> p2
               | None -> None)

  (** val meet : t -> t -> t Core.Base.imp **)

  let meet p1 p2 =
    match p1 with
    | Some p1' ->
      (match p2 with
       | Some p2' ->
         let l1,l2 = Cs.wrap2 p1'.cons p2'.cons in
         let wp1 = { lcf = Cs.certCstrLCF; backend = p1'.ml; cert = l1 } in
         let opt,cert0 = meet (wp1,(p2'.ml,l2)) in
         let res = Cs.unwrap cert0 in
         (match opt with
          | Some p' -> Some { cons = res; ml = p' }
          | None ->
            if Cs.isContrad res
            then None
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   CERT
                   ('P'::('e'::('d'::('r'::('a'::('Q'::('.'::('m'::('e'::('e'::('t'::[])))))))))))
                   p1)
       | None -> None)
    | None -> None

  (** val widen : t -> t -> t Core.Base.imp **)

  let widen p1 p2 =
    match p1 with
    | Some pol1 ->
      (match p2 with
       | Some pol2 ->
         let cs,shadow = widen (pol1.ml,pol2.ml) in
         Some { cons = shadow; ml = cs }
       | None -> p1)
    | None -> (match p2 with
               | Some _ -> p2
               | None -> None)

  (** val project : t -> PVar.t -> t Core.Base.imp **)

  let project p x =
    match p with
    | Some p1 ->
      let shadow,cert0 = project ((wrap p1),x) in
      let res = Cs.unwrap cert0 in
      if Cs.isFree x res
      then Some { cons = res; ml = shadow }
      else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
             CERT
             (append
               ('p'::('r'::('o'::('j'::('e'::('c'::('t'::(':'::(' '::[])))))))))
               (append (PVar.pr x)
                 (append
                   (' '::('n'::('o'::('t'::(' '::('f'::('r'::('e'::('e'::(' '::('i'::('n'::(' '::[])))))))))))))
                   (Cs.pr res)))) top
    | None -> None

  (** val affAssume :
      cmpG -> QAffTerm.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let affAssume cmp aft a =
    match cmpG2T cmp with
    | Some cmpT -> CstrD.assume (QAtomicCond.toCstr cmpT aft) a
    | None ->
      BasicD.join (CstrD.assume (QAtomicCond.toCstr LtT aft) a)
        (CstrD.assume (QAtomicCond.toCstr LtT (QAffTerm.opp aft)) a)

  (** val applyHandelman_one :
      cmpG -> QTerm.t -> BasicD.polT -> BasicD.t -> Handelman_compute.certif
      -> BasicD.t Core.Base.imp **)

  let applyHandelman_one cmp qt pol0 a cert0 =
    print_endline(Printf.sprintf "apply_handelman: %s "(polPr pol0 |> CoqPr.charListTr));
    let aff = Handelman_compute.eq_witness pol0.BasicD.cons cert0 qt in
    affAssume cmp aff a

  (** val f :
      cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif -> BasicD.t
      Core.Base.imp -> BasicD.t Core.Base.imp **)

  let f cmp qt pol0 cert0 a =
    applyHandelman_one cmp qt pol0 a cert0

  (** val applyHandelman :
      cmpG -> QTerm.t -> BasicD.polT -> Handelman_compute.certif list ->
      BasicD.t Core.Base.imp **)

  let applyHandelman cmp qt pol0 certs =
    fold_right (f cmp qt pol0) (Some pol0) certs

  (** val assume_eq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp **)

  let assume_eq qt pol0 =
    let opp0 = QTerm.Opp qt in
    BasicD.meet
      (applyHandelman Le qt pol0 (handelman_oracle pol0.BasicD.ml Le qt))
      (applyHandelman Le opp0 pol0 (handelman_oracle pol0.BasicD.ml Le opp0))

  (** val assume_neq : QTerm.t -> BasicD.polT -> BasicD.t Core.Base.imp **)

  let assume_neq qt pol0 =
    let opp0 = QTerm.Opp qt in
    BasicD.join
      (applyHandelman Lt qt pol0 (handelman_oracle pol0.BasicD.ml Lt qt))
      (applyHandelman Lt opp0 pol0 (handelman_oracle pol0.BasicD.ml Lt opp0))

  (** val assume : QAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp **)

  let assume c a =
    let ti = c.QAtomicCond.right in
    let te,aft = QTerm.affineDecompose ti in
    (match te with
     | QTerm.Cte c0 ->
       if let filtered_var = (this c0).coq_Qnum in
          (match filtered_var with
           | Z0 -> true
           | _ -> false)
       then affAssume c.QAtomicCond.cmpOp aft a
       else (match a with
             | Some pol0 ->
               (match c.QAtomicCond.cmpOp with
                | Eq -> assume_eq ti pol0
                | Neq -> assume_neq ti pol0
                | _ ->
                  applyHandelman c.QAtomicCond.cmpOp ti pol0
                    (handelman_oracle pol0.BasicD.ml c.QAtomicCond.cmpOp ti))
             | None -> None)
     | _ ->
       (match a with
        | Some pol0 ->
          (match c.QAtomicCond.cmpOp with
           | Eq -> assume_eq ti pol0
           | Neq -> assume_neq ti pol0
           | _ ->
             applyHandelman c.QAtomicCond.cmpOp ti pol0
               (handelman_oracle pol0.BasicD.ml c.QAtomicCond.cmpOp ti))
        | None -> None))
 end

module FullDom =
 MakeFull(QNum)(QCond)(QItv)(QAtomicCond)(AtomicD)(AtomicD)(Rename)(ItvD)(AtomicD)
