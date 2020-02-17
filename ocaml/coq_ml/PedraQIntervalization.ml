module Cstr_BE = Cstr_type
open ASAtomicCond
open ASTerm
open Debugging
open DomainInterfaces
open NumC
open ProgVar
open WrapperTraductors

module CQAffTerm = LinTerm.QAffTerm

module Lift (LHD: QInterface.LowLevelDomain) = struct

    module AtomicD = struct

        include LHD

        let isBottom = is_bottom

        let isIncl = leq

        let project p x =  project [export_certvar x] p

        let rename x y = rename (export_certvar x) (export_certvar y)

        let pr p = CoqPr.stringTr (to_string Var.to_string p)

        let to_string f p = CoqPr.stringTr (to_string (fun x -> CoqPr.charListTr (f (import_certvar x))) p)

        (*let assume c = assume [export_cmpT c.QAtomicCond.cmpOp, export_CQTerm c.QAtomicCond.right]*)
    end

    include AtomicD

    module Oracle = IntervalizationOracle.OracleQ

    module QNItv = IOtypes.DomainQ.NoneItv

    module NAItv = IOtypes.DomainQ.NAItv


    let export_bnd : QItv.bndT -> QNoneItv.QN.t
    = function
    | QItv.Infty -> None
    | QItv.Open q -> Some q
    | QItv.Closed q -> Some q

    let export_qitv : QItv.t -> QNItv.t
    = fun itv -> {
        QNItv.low = export_bnd itv.QItv.lower ;
        up = export_bnd itv.QItv.upper;
    }


    let is_pos : QNum.t -> bool
    = QNum.ltLeDec QNum.z


    open ImpureConfig
    open PredTrans

    module G = struct
        type cdac = t -> t Core.Base.imp
        (* singleton inductive, whose constructor was raw_build_cdac *)


        (** val impl : cdac -> t -> t Core.Base.imp **)

        let impl c = c

        (** val build_cdac :
          (t -> t Core.Base.imp) -> (ZNum.t Mem.t -> ZNum.t
          Mem.t MPP_Definitions.coq_MPP) -> cdac **)

        let build_cdac impl0 _ = impl0

        (** val spec :
          cdac -> ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP **)

        let spec _ _ = MPP_Definitions.Build_MPP

        (** val cast :
          cdac -> (ZNum.t Mem.t -> ZNum.t Mem.t MPP_Definitions.coq_MPP) ->
          cdac **)

        let cast gc spec' =
            build_cdac (impl gc) spec'

        (** val skip : cdac **)

        let skip : cdac =
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
          if isBottom a' then a' else impl gc2 a')
          (coq_Seq (spec gc1) (spec gc2))

        (** val join : cdac -> cdac -> cdac **)

        let join gc1 gc2 =
        build_cdac (fun a -> join (impl gc1 a) (impl gc2 a))
          (coq_Join (spec gc1) (spec gc2))

        (** val loop : cdac -> (t -> t Core.Base.imp) -> cdac **)

        let loop gc oracle0 =
        build_cdac (fun a ->
          let inv = oracle0 a in
          if isIncl a inv
          then if isIncl (impl gc inv) inv
               then inv
               else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                      DEMO
                      ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('p'::('r'::('e'::('s'::('e'::('r'::('v'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
                      top
          else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                 DEMO
                 ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('i'::('n'::('i'::('t'::[]))))))))))))))
                 top)
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
          (t -> 'a1 Core.Base.imp) -> ('a1 -> cdac) -> __ -> cdac **)

        let bind ge gc _ =
        build_cdac (fun a -> impl (gc (ge a)) a)
          (coq_UMeet (fun x -> coq_Seq coq_Assert (spec (gc x))))

    end


    (* LHD.assume : (cmpT * Term.t) list -> t -> t *)

    let affAssumeEq aft a =
        let (vec,cste) = export_QAffTerm aft in
        let cstr = Pol.Cs.mk2 Cstr_BE.Eq vec (Scalar.Rat.neg cste) in
        LHD.assume [Cstr_BE.EQ, Term.of_cstr cstr] a

    let affAssumeLe aft a =
        let (vec,cste) = export_QAffTerm aft in
        let cstr = Pol.Cs.mk2 Cstr_BE.Le vec (Scalar.Rat.neg cste) in
        LHD.assume [Cstr_BE.LE, Term.of_cstr cstr] a

    let affAssumeLt aft a =
        let (vec,cste) = export_QAffTerm aft in
        let cstr = Pol.Cs.mk2 Cstr_BE.Lt vec (Scalar.Rat.neg cste) in
        LHD.assume [Cstr_BE.LT, Term.of_cstr cstr] a

    let affAssumeGt aft a =
        affAssumeLt (CQAffTerm.opp aft) a

    let affAssume cmp0 aft a =
    match cmp0 with
    | Eq -> affAssumeEq aft a
    | Le -> affAssumeLe aft a
    | Lt -> affAssumeLt aft a
    | Neq -> join (affAssumeLt aft a) (affAssumeGt aft a)

    open FMapPositive

    (*
    module QNItvD = struct

        let getItvMode
        = fun mo te a ->
        let te0,aft = CQTerm.affineDecompose te in
        (match te0 with
         | CQTerm.Cte c ->
           (if QNum.isZero c
            then QNItv.fromQItv
                (ItvD.getItvMode mo (CQTerm.fromAff aft) a)
            else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                INTERN
                ('g'::('e'::('t'::('_'::('i'::('t'::('v'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[]))))))))))))))))))))))))
                QNItv.top)
         | _ ->
           (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
             INTERN
             ('g'::('e'::('t'::('_'::('i'::('t'::('v'::(':'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::[]))))))))))))))))))))))))
             QNItv.top)
    end
    *)

    let import_QbndT bnd =
        WrapperTraductors.import_QbndT bnd
        |> QNItv.fromBndT

    let getItvMode mo t p =
    match mo with
    | BOTH ->
        let itv = itvize p (export_QTerm t) in
        { QNItv.low = import_QbndT itv.Pol.low ;
        QNItv.up = import_QbndT itv.Pol.up }
    | UP -> begin
        match getUpperBound p (export_QTerm t) with
        | Some bound -> {
            QNItv.low = None ;
            QNItv.up = import_QbndT bound}
        | None -> failwith "empty"
    end
    | LOW -> begin
        match getLowerBound p (export_QTerm t) with
        | Some bound -> {
        QNItv.low = import_QbndT bound  ;
        QNItv.up = None}
        | None -> failwith "empty"
    end

    let rec intervalize : (PVar.t -> QNItv.t) -> bool -> mode -> CQTerm.term -> t -> QNItv.t Core.Base.imp
        = fun env0 sic mo te a ->
        match te with
        | CQTerm.Var x ->
         if sic then env0 x else getItvMode mo (CQTerm.Var x) a
        | CQTerm.Cte c -> QNItv.single c
        | CQTerm.Add (tl, tr) ->
         QNItv.add mo (intervalize env0 sic mo tl a)
           (intervalize env0 sic mo tr a)
        | CQTerm.Opp te0 ->
         QNItv.opp (intervalize env0 sic (QNItv.oppMode mo) te0 a)
        | CQTerm.Mul (tl, tr) ->
         (match let rec matchCte0 = function
                | CQTerm.Cte c -> Some c
                | CQTerm.Annot (_, te1) -> matchCte0 te1
                | _ -> None
                in matchCte0 tr with
          | Some c ->
            let i =
              intervalize env0 sic
                (if is_pos c then mo else QNItv.oppMode mo) tl a
            in
            if is_pos c
            then (match mo with
                  | BOTH ->
                    { QNItv.low =
                      (if QNum.isZero c
                       then Some QNum.z
                       else (match i.QNItv.low with
                             | Some z2 -> Some (QNum.mul c z2)
                             | None -> None)); QNItv.up =
                      (if QNum.isZero c
                       then Some QNum.z
                       else (match i.QNItv.up with
                             | Some z2 -> Some (QNum.mul c z2)
                             | None -> None)) }
                  | UP ->
                    { QNItv.low = None; QNItv.up =
                      (if QNum.isZero c
                       then Some QNum.z
                       else (match i.QNItv.up with
                             | Some z2 -> Some (QNum.mul c z2)
                             | None -> None)) }
                  | LOW ->
                    { QNItv.low =
                      (if QNum.isZero c
                       then Some QNum.z
                       else (match i.QNItv.low with
                             | Some z2 -> Some (QNum.mul c z2)
                             | None -> None)); QNItv.up = None })
            else (match mo with
                  | BOTH ->
                    { QNItv.low =
                      (match i.QNItv.up with
                       | Some z2 -> Some (QNum.mul c z2)
                       | None -> None); QNItv.up =
                      (match i.QNItv.low with
                       | Some z2 -> Some (QNum.mul c z2)
                       | None -> None) }
                  | UP ->
                    { QNItv.low = None; QNItv.up =
                      (match i.QNItv.low with
                       | Some z2 -> Some (QNum.mul c z2)
                       | None -> None) }
                  | LOW ->
                    { QNItv.low =
                      (match i.QNItv.up with
                       | Some z2 -> Some (QNum.mul c z2)
                       | None -> None); QNItv.up = None })
          | None ->
            QNItv.mul mo (intervalize env0 sic BOTH tl a)
              (intervalize env0 sic BOTH tr a))
        | CQTerm.Annot (a0, te0) ->
         (match a0 with
          | CQTerm.Annot.AFFINE ->
            if sic
            then intervalize env0 sic mo te0 a
            else getItvMode mo te0 a
          | CQTerm.Annot.STATIC ->
            let te1 =
              (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
                DEBUG
                ('!'::('I'::('N'::('T'::('E'::('R'::('V'::(' '::('S'::('T'::('A'::('T'::('I'::('C'::('!'::[])))))))))))))))
                te0
            in
            intervalize env0 true mo te1 a
          | _ -> intervalize env0 sic mo te0 a)


    let add_variable a x mitv =
    match PositiveMap.find x mitv with
    | Some _ -> mitv
    | None ->
     PositiveMap.add x (getItvMode BOTH (CQTerm.Var x) a) mitv

    let get_variables a te =
    let rec fold_variables0 te0 f i =
     match te0 with
     | CQTerm.Var x -> f x i
     | CQTerm.Cte _ -> i
     | CQTerm.Add (tl, tr) -> fold_variables0 tl f (fold_variables0 tr f i)
     | CQTerm.Opp te1 -> fold_variables0 te1 f i
     | CQTerm.Mul (tl, tr) -> fold_variables0 tl f (fold_variables0 tr f i)
     | CQTerm.Annot (_, te1) -> fold_variables0 te1 f i
    in fold_variables0 te (fun x i -> add_variable a x i) PositiveMap.Leaf

    let mitv_find mitv x =
    match PositiveMap.find x mitv with
    | Some itv0 -> itv0
    | None -> QNItv.top

    let gAffAssumeLe aft =
    G.build_cdac (affAssumeLe aft) coq_Assume

    let gAffAssumeLt aft =
    G.build_cdac (affAssumeLt aft) coq_Assume

    let gAffAssumeGt aft =
    G.build_cdac (affAssumeGt aft) coq_Assume

    let gIntervalize env0 sic mo te k =
    G.bind (intervalize env0 sic mo te) k __

    let intervalizeG env0 sic mo te k =
    G.cast (gIntervalize env0 sic mo te (fun i -> k (NAItv.cte i)))
    (coq_Seq coq_Assume
      (coq_UMeet (fun itv0 -> coq_Seq coq_Assert (G.spec (k itv0)))))

    let castAFFINE_error =
    'A'::('S'::('A'::('t'::('o'::('m'::('i'::('c'::('C'::('o'::('n'::('d'::('.'::('c'::('a'::('s'::('t'::('A'::('F'::('F'::('I'::('N'::('E'::(' '::('o'::('v'::('e'::('r'::(' '::('n'::('o'::('n'::('-'::('a'::('f'::('f'::('i'::('n'::('e'::(' '::('t'::('e'::('r'::('m'::(' '::[]))))))))))))))))))))))))))))))))))))))))))))

    let castAFFINE : BasicQTerm.term -> (CQAffTerm.t -> G.cdac) -> G.cdac
        = fun te k ->
        G.cast
        (let te',aft = CQTerm.affineDecompose te in
         (match te' with
          | CQTerm.Cte c ->
            (if QNum.isZero c
            then k aft
            else G.fail (String0.append castAFFINE_error (CQTerm.pr te)))
          | _ -> G.fail (String0.append castAFFINE_error (CQTerm.pr te))))
        (coq_Meet coq_Skip
          (coq_UMeet (fun aft -> coq_Seq coq_Assert (G.spec (k aft)))))

    let caseSign : BasicQTerm.term -> (CQAffTerm.t -> G.cdac) -> (CQAffTerm.t -> G.cdac) -> G.cdac
        = fun te aP aN ->
        castAFFINE te (fun aft ->
        G.join (G.seq (gAffAssumeLe aft) (aP aft))
          (G.seq (gAffAssumeGt aft) (aN aft)))

    let linearizeMul : (PVar.t -> QNItv.t) -> bool -> mode -> CQTerm.term -> CQTerm.term ->
        (NAItv.itv -> G.cdac) -> G.cdac
        = fun env0 sic mo tl tr k ->
        let omo = QNItv.oppMode mo in
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

    let rec linearizeG env0 sic mo te k =
    match te with
    | CQTerm.Add (tl, tr) ->
    linearizeG env0 sic mo tl (fun itvl ->
      linearizeG env0 sic mo tr (fun itvr -> k (NAItv.add mo itvl itvr)))
    | CQTerm.Opp te0 ->
    linearizeG env0 sic (QNItv.oppMode mo) te0 (fun itv0 ->
      k (NAItv.opp mo itv0))
    | CQTerm.Mul (tl, tr0) ->
    (match tr0 with
     | CQTerm.Annot (a, tr) ->
       (match a with
        | CQTerm.Annot.AFFINE ->
          (match let rec matchCte0 = function
                 | CQTerm.Cte c -> Some c
                 | CQTerm.Annot (_, te1) -> matchCte0 te1
                 | _ -> None
                 in matchCte0 tr with
           | Some c ->
             linearizeG env0 sic
               (if is_pos c then mo else QNItv.oppMode mo) tl
               (fun itv0 -> k (NAItv.mulZ mo c itv0))
           | None -> linearizeMul env0 sic mo tl tr k)
        | _ -> intervalizeG env0 sic mo te k)
     | _ -> intervalizeG env0 sic mo te k)
    | CQTerm.Annot (a, te0) ->
    (match a with
     | CQTerm.Annot.AFFINE ->
       castAFFINE te0 (fun aft -> k (NAItv.single aft))
     | CQTerm.Annot.INTERV -> intervalizeG env0 sic mo te0 k
     | CQTerm.Annot.STATIC -> linearizeG env0 true mo te0 k
     | _ -> linearizeG env0 sic mo te0 k)
    | _ -> intervalizeG env0 sic mo te k

    let linearizeGX env0 sic mo te k =
    G.cast (linearizeG env0 sic mo te k)
    (coq_Seq coq_Assume
      (coq_Meet coq_Skip
        (coq_UMeet (fun itv0 -> coq_Seq coq_Assert (G.spec (k itv0))))))

    let assumeOpCPS env0 sic cmp0 te aft =
    let te0 =
    (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
      DEBUG
      (String0.append
        ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('t'::('e'::(':'::[]))))))))))))
        (CQTerm.pr te)) te
    in
    let te1 =
    (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
      DEBUG
      (String0.append
        ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('a'::('f'::('t'::(':'::[])))))))))))))
        (CQTerm.pr
          (CQTerm.smartAdd (CQTerm.fromLin aft.CQAffTerm.lin) (CQTerm.Cte
            aft.CQAffTerm.cte)))) te0
    in
    G.cast
    (match cmp0 with
     | Eq ->
       linearizeGX env0 sic BOTH te1 (fun i ->
         G.seq
           (G.coq_try i.NAItv.up (fun u ->
             gAffAssumeLe (CQAffTerm.add u aft)))
           (G.coq_try i.NAItv.low (fun l ->
             gAffAssumeLe (CQAffTerm.opp (CQAffTerm.add l aft)))))
     | Le ->
       linearizeGX env0 sic UP te1 (fun i ->
         G.coq_try i.NAItv.up (fun u -> gAffAssumeLe (CQAffTerm.add u aft)))
     | Lt ->
       linearizeGX env0 sic UP te1 (fun i ->
         G.coq_try i.NAItv.up (fun u -> gAffAssumeLt (CQAffTerm.add u aft)))
     | Neq ->
       linearizeGX env0 sic BOTH te1 (fun i ->
         G.join
           (G.coq_try i.NAItv.up (fun u ->
             gAffAssumeLt (CQAffTerm.add u aft)))
           (G.coq_try i.NAItv.low (fun l ->
             gAffAssumeGt (CQAffTerm.add l aft))))) coq_Assume

    let assumeOpAnnot env0 sic cmp0 te aft =
    G.cast (assumeOpCPS env0 sic cmp0 (CQTerm.annotAFFINE te) aft) coq_Assume

    module QPeq = QPomialEquality(CQTerm)(CQTerm)

    let test_eq te te' =
    if QPeq.pomial_eq te te'
    then te'
    else (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
         CERT
         ('L'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('e'::('O'::('r'::('a'::('c'::('l'::('e'::('B'::('u'::('g'::(' '::('?'::(' '::('T'::('h'::('e'::(' '::('t'::('w'::('o'::(' '::('p'::('o'::('l'::('y'::('n'::('o'::('m'::('i'::('a'::('l'::(' '::('d'::('i'::('f'::('f'::('e'::('r'::('s'::('.'::('.'::('.'::[]))))))))))))))))))))))))))))))))))))))))))))))))))
         te

    let rec skip_oracle = function
    | CQTerm.Opp te0 -> skip_oracle te0
    | CQTerm.Annot (a, te0) ->
    (match a with
    | CQTerm.Annot.SKIP_ORACLE ->
     (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
       DEBUG
       ('O'::('R'::('A'::('C'::('L'::('E'::(' '::('S'::('K'::('I'::('P'::('P'::('E'::('D'::(' '::('!'::[]))))))))))))))))
       true
    | _ -> skip_oracle te0)
    | _ -> false

    let assumeOpFromOracle env0 sic lc cmp0 te aft =
    G.cast
    (G.bind (fun _ -> IntervalizationOracle.oracleQ lc) (fun te0 ->
      if skip_oracle te0
      then G.skip
      else let te' = test_eq te te0 in assumeOpAnnot env0 sic cmp0 te' aft)
      __) coq_Assume

    let assumeOp2 env0 sic lc cmp0 te aft =
    G.cast
    (G.seq
      (assumeOpCPS env0 true cmp0 (CQTerm.Annot (TopLevelAnnot.INTERV,
        te)) aft) (assumeOpFromOracle env0 sic lc cmp0 te aft)) coq_Assume

    let assumeOp sic cmp0 te aft ti a =
    let env0 = mitv_find (get_variables a te) in
    let lc : Oracle.linearizeContext = { Oracle.nonaffine = te;
      Oracle.env = env0;
      Oracle.affine = aft;
      Oracle.source = ti;
      Oracle.cmp = cmp0 }
    in
    let te0 =
    (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
      DEBUG
      (String0.append
        ('a'::('s'::('s'::('u'::('m'::('e'::('O'::('p'::('.'::('t'::('i'::(':'::[]))))))))))))
        (CQTerm.pr ti)) te
    in
    if skip_oracle te0
    then G.impl (assumeOpAnnot env0 sic cmp0 te0 aft) a
    else G.impl (assumeOp2 env0 sic lc cmp0 te0 aft) a

    (** val assume : QAtomicCond.t -> BasicD.t -> BasicD.t Core.Base.imp **)
    let assume c a =
        match !Flags.lin with
        | Flags.Intervalization | Flags.Both -> begin
            let ti = c.QAtomicCond.right in
            let te,aft = CQTerm.affineDecompose ti in
            match te with
                | CQTerm.Cte c0 -> begin
                    if QNum.isZero c0
                    then affAssume c.QAtomicCond.cmpOp aft a
                    else
                        let a' = assumeOp false c.QAtomicCond.cmpOp te aft ti a in
                        if !Flags.lin = Flags.Both
                        then LHD.assume [export_cmpT c.QAtomicCond.cmpOp, export_QTerm c.QAtomicCond.right] a'
                        else a'
                    end
                | _ -> begin
                    let a' = assumeOp false c.QAtomicCond.cmpOp te aft ti a in
                    if !Flags.lin = Flags.Both
                    then LHD.assume [export_cmpT c.QAtomicCond.cmpOp, export_QTerm c.QAtomicCond.right] a'
                    else a'
                end
        end
        | Flags.Handelman -> LHD.assume [export_cmpT c.QAtomicCond.cmpOp, export_QTerm c.QAtomicCond.right] a
(*
    let getItvMode mo te a =
        intervalize (fun _ -> QNItv.top) false mo (CQTerm.annotAFFINE te) a
*)

    let getItvMode mo t p =
    match mo with
    | BOTH ->
        let itv = itvize p (export_QTerm t) in
        { QItv.lower = WrapperTraductors.import_QbndT itv.Pol.low ;
        QItv.upper = WrapperTraductors.import_QbndT itv.Pol.up }
    | UP -> begin
        match getUpperBound p (export_QTerm t) with
        | Some bound -> {
            QItv.lower = QItv.Infty ;
            QItv.upper = WrapperTraductors.import_QbndT bound}
        | None -> failwith "empty"
        end
    | LOW -> begin
        match getLowerBound p (export_QTerm t) with
        | Some bound -> {
            QItv.lower = WrapperTraductors.import_QbndT bound  ;
            QItv.upper = QItv.Infty}
        | None -> failwith "empty"
        end
end
