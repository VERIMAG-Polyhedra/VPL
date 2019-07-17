(*****************************************)
(* Implementation of the backend API     *)
(* see PedraQBackend.v                   *)
(*                                       *)
(*****************************************)


module Nb = Scalar.Rat
module Var = Var.Positive
module Vec = Vector.Rat.Positive
module Cs = Cstr.Rat.Positive
module Cons = IneqSet.Cons
module Cert = IneqSet.Cert
module EqSet = IneqSet.EqSet

(* Preliminary functions:
    frontend data-structures <-> backend data-structures
 *)
let coqPosToZ: BinNums.positive -> Nb.Z.t
= fun p0 -> CoqPr.posTr p0

let zToCoqPos: Nb.Z.t -> BinNums.positive
= fun z0 ->
	if Nb.Z.cmp Nb.Z.z z0 >= 0 then
		invalid_arg "Support.zToCoqPos"
	else
		let rec f (z: Nb.Z.t): BinNums.positive =
			if Nb.Z.cmp z Nb.Z.u = 0 then
				BinNums.Coq_xH
			else
				if Nb.Z.cmp (Nb.Z.lAnd z Nb.Z.u) Nb.Z.u = 0 then
					BinNums.Coq_xI (f (Nb.Z.shiftR z 1))
				else
					BinNums.Coq_xO (f (Nb.Z.shiftR z 1))
		in
		f z0

let coqZToZ: BinNums.coq_Z -> Nb.Z.t
= fun z -> CoqPr.zTr z

let zToCoqZ: Nb.Z.t -> BinNums.coq_Z
= fun z ->
	let sign = Nb.Z.cmp Nb.Z.z z in
	if sign = 0 then
		BinNums.Z0
	else
		if sign < 0 then
			BinNums.Zpos (zToCoqPos z)
		else
			BinNums.Zneg (zToCoqPos (Nb.Z.neg z))

let nToNb: NumC.QNum.t -> Nb.t
= fun q ->
	let num = coqZToZ (QArith_base.coq_Qnum q) in
	let den = coqPosToZ (QArith_base.coq_Qden q) in
	Nb.ofZ num den

let nToNumC: Nb.t -> NumC.QNum.t
= fun n ->
	let (num, den) = Nb.toZ n in
	let q = {
		QArith_base.coq_Qnum = zToCoqZ num;
		QArith_base.coq_Qden = zToCoqPos den }
	in
	Qcanon.coq_Q2Qc q

let coq_QToNb: QArith_base.coq_Q -> Nb.t
	= nToNb

let nToCoq_Q: Nb.t -> QArith_base.coq_Q
	= fun n ->
	let (num, den) = Nb.toZ n in
	{
		QArith_base.coq_Qnum = zToCoqZ num;
		QArith_base.coq_Qden = zToCoqPos den }

let progVarToVar: ProgVar.PVar.t -> Var.t
= fun x ->
	let rec posToVar: BinNums.positive -> Var.t
	= function
		| BinNums.Coq_xH -> Var.XH
		| BinNums.Coq_xO p -> Var.XO (posToVar p)
		| BinNums.Coq_xI p -> Var.XI (posToVar p)
	in
	posToVar (ProgVar.PVar.export x)

let varToProgVar: Var.t -> ProgVar.PVar.t
= fun x ->
	let rec varToPos: Var.t -> BinNums.positive
	= function
		| Var.XH -> BinNums.Coq_xH
		| Var.XI p -> BinNums.Coq_xI (varToPos p)
		| Var.XO p -> BinNums.Coq_xO (varToPos p)
	in
	ProgVar.PVar.import (varToPos x)

let ltToVec: LinTerm.LinQ.t -> Vec.t
= fun lt ->
	let convert: ProgVar.PVar.t * NumC.QNum.t -> Nb.t * Var.t
	= fun (x, n) -> (nToNb n, progVarToVar x)
	in
	Vec.mk (List.map convert (LinTerm.LinQ.export lt))

let vecToLt: Vec.t -> LinTerm.LinQ.t
= fun v ->
(*
	let rmZeroes: (Var.t * Nb.t) list -> (Var.t * Nb.t) list
	= fun l -> List.filter (fun (_, n) -> Nb.cmpz n <> 0) l
	in
*)
	let convert: Var.t * Nb.t -> ProgVar.PVar.t * NumC.QNum.t
	= fun (x, n) -> (varToProgVar x, nToNumC n)
	in
	LinTerm.LinQ.import (List.map convert (Rtree.toList v))

let cToCmpT: NumC.cmpT -> Cstr.cmpT
= function
	| NumC.EqT -> Cstr.Eq
	| NumC.LeT -> Cstr.Le
	| NumC.LtT -> Cstr.Lt

let cToCmp: Cstr.cmpT -> NumC.cmpT
= function
	| Cstr.Eq -> NumC.EqT
	| Cstr.Le -> NumC.LeT
	| Cstr.Lt -> NumC.LtT

let cToCstr: CstrC.Cstr.t -> Cs.t
= fun c -> {
	Cs.v = ltToVec (CstrC.Cstr.coefs c);
	Cs.typ = cToCmpT (CstrC.Cstr.typ c);
	Cs.c = nToNb (CstrC.Cstr.cst c)
}

let cToCstrC: Cs.t -> CstrC.Cstr.t
= fun c -> {
	CstrC.Cstr.typ = cToCmp (Cs.get_typ c);
	CstrC.Cstr.coefs = vecToLt (Cs.get_v c);
	CstrC.Cstr.cst = nToNumC (Cs.get_c c)
}

let cstrCPr: CstrC.Cstr.t -> string
= fun c -> CoqPr.charListTr (CstrC.Cstr.pr c)

let cToConsC: 'c Cons.t -> CstrC.Cstr.t
= fun c -> cToCstrC (Cons.get_c c)

let unimplemented: string -> 'a -> 'b
= fun s _ -> failwith (Printf.sprintf "PedraQOracles.%s not yet fully implemented" s)

(* TODO: A REVOIR *)
let pToConsSet: 'c Pol.t -> ConsSet.Cs.t
= fun p ->
  let e = EqSet.list (Pol.get_eqs p) in
  let i = IneqSet.list (Pol.get_ineqs p) in
  unimplemented "pToConsSet" (List.map cToConsC (List.append e i))

(***********************************************)
(* Link between backend and frontend polyhedra *)
(***********************************************)
open CstrLCF

let debug = false

type 'c pedraCert = (unit Pol.t, CstrC.Cstr.t, 'c) CstrLCF.pedraInput

let cstrLCF_from_frontend: (CstrC.Cstr.t, 'c) CstrLCF.cstrLCF -> 'c Cert.t
= fun lcf ->
  { Cert.name = "LCF from frontend";
    Cert.top = lcf.top;
    Cert.triv = (fun t n -> lcf.triv (cToCmp t) (nToNumC n));
    Cert.add = lcf.add;
    Cert.mul = (fun n c -> lcf.mul (nToNumC n) c);
    Cert.merge = lcf.merge;
    Cert.to_le = lcf.to_le;
    Cert.to_string = (fun c -> cstrCPr (lcf.export c)); (* TO IMPROVE ? *)
    Cert.rename = (fun _ -> failwith "No rename in frontend's LCF")
  }

let trivLCF: unit Cert.t
= { Cert.name = "Trivial LCF";
    Cert.top = ();
    Cert.triv = (fun _ _ -> ());
    Cert.add = (fun _ _ -> ());
    Cert.mul = (fun _ _ -> ());
    Cert.merge = (fun _ _ -> ());
    Cert.to_le = (fun _ -> ());
    Cert.to_string = (fun _ -> "");
    Cert.rename = (fun _ _ _ -> ())
  }

(*** IMPORT certificates into backend representation
**)

let direct_import: (CstrC.Cstr.t, 'c) CstrLCF.cstrLCF -> ('c list) -> ('c Cons.t) list
= let rec direct_import lcf l acc =
  match l with
  | [] -> acc
  | c::l' -> direct_import lcf l' ((cToCstr (lcf.export c), c)::acc)
  in fun lcf l -> direct_import lcf l []

let check_cstr_synchro: 'c Cert.t -> Cs.t -> 'c -> bool
= fun lcf c cert ->
  let actual = lcf.Cert.to_string cert in
  let expected = cstrCPr (cToCstrC c) in
  if actual <> expected then (
    Printf.printf "failed synchro: %s versus %s expected\n" actual expected;
    false
  ) else (
    true
  )

let rec ineqs_import: 'c Cert.t -> (unit Cons.t) list -> 'c list -> ('c Cons.t) list -> ('c Cons.t) list * ('c list)
= fun lcf p l acc ->
  match p with
  | [] -> (acc, l)
  | (c,_)::p' ->
    match l with
    | [] -> assert false
    | cert::l' ->
      (* check invariant *)
      assert (not debug || check_cstr_synchro lcf c cert);
      ineqs_import lcf p' l' ((c, cert)::acc)

let rec eqs_import: 'c Cert.t -> ('a * unit Cons.t) list -> 'c list -> ('a * 'c Cons.t) list -> ('a * 'c Cons.t) list * ('c list)
= fun lcf p l acc ->
  match p with
  | [] -> (acc, l)
  | (a, (c,_))::p' ->
    match l with
    | [] -> assert false
    | cert::l' ->
      (* check invariant *)
      assert (not debug || check_cstr_synchro lcf c cert);
      eqs_import lcf p' l' ((a, (c, cert))::acc)

let import: 'c Cert.t -> unit Pol.t -> 'c list -> 'c Pol.t
= fun lcf p l ->
  let (eqs, l0) = eqs_import lcf p.Pol.eqs l [] in
  let (ineqs, l1) = ineqs_import lcf p.Pol.ineqs l0 [] in
  assert (l1=[]);
  {Pol.eqs = eqs; Pol.ineqs = ineqs}

(*** EXPORT certificates from backend representation
**)

let rec ineqs_export: ('c Cons.t) list -> (unit Cons.t) list -> 'c list -> ((unit Cons.t) list) * ('c list)
= fun p acc1 acc2 ->
  match p with
  | [] -> (acc1, acc2)
  | (c,ce)::p' ->
    ineqs_export p' ((c,())::acc1) (ce::acc2)

let rec eqs_export: ('a * 'c Cons.t) list ->  ('a * unit Cons.t) list -> 'c list ->  (('a * unit Cons.t) list) * ('c list)
= fun p acc1 acc2 ->
  match p with
  | [] -> (acc1, acc2)
  | (a,(c,ce))::p' -> eqs_export p' ((a, (c,()))::acc1) (ce::acc2);;

let export: 'c Pol.t -> (unit Pol.t) * ('c list)
= fun p ->
  let (ineqs, l1) = ineqs_export p.Pol.ineqs [] [] in
  let (eqs, l2) = eqs_export p.Pol.eqs [] l1 in
  ({Pol.eqs = eqs; Pol.ineqs = ineqs}, l2)

(* a join export *)
let rec join_ineqs_export: ('c1 -> 'c2 -> 'c3) -> ('c1 Cons.t) list -> ('c2 Cons.t) list -> (unit Cons.t) list -> 'c3 list -> ((unit Cons.t) list) * ('c3 list)
= fun unify p1 p2 acc1 acc2 ->
  match p1, p2 with
  | [], [] -> (acc1, acc2)
  | (c, ce1)::p1', (_,ce2)::p2' ->
     join_ineqs_export unify p1' p2' ((c,())::acc1) ((unify ce1 ce2)::acc2)
  | _, _ -> failwith "join_ineqs_export: lists of constraints of incompatible size"
  
let rec join_eqs_export: ('c1 -> 'c2 -> 'c3) -> ('a * 'c1 Cons.t) list -> ('b*'c2 Cons.t) list -> ('a*unit Cons.t) list -> 'c3 list -> (('a * unit Cons.t) list) * ('c3 list)
= fun unify p1 p2 acc1 acc2 ->
  match p1, p2 with
  | [], [] -> (acc1, acc2)
  | (a, (c, ce1))::p1', (_,(_,ce2))::p2' ->
     join_eqs_export unify p1' p2' ((a, (c,()))::acc1) ((unify ce1 ce2)::acc2)
  | _, _ -> failwith "join_eqs_export: lists of constraints of incompatible size"

let join_export: ('c1 -> 'c2 -> 'c3) -> 'c1 Pol.t -> 'c2 Pol.t -> (unit Pol.t) * ('c3 list)
= fun unify p1 p2 ->
  let (ineqs, l1) = join_ineqs_export unify p1.Pol.ineqs p2.Pol.ineqs [] [] in
  let (eqs, l2) = join_eqs_export unify p1.Pol.eqs p2.Pol.eqs [] l1 in
  ({Pol.eqs = eqs; Pol.ineqs = ineqs}, l2)

(**********************************************)
(* Actual beginning of the API implementation *)
(**********************************************)

type t = unit Pol.t

let top: t
= Pol.top

(* [isEmpty p]
 - returns [None]  (if [p] is not empty)
 - or returns [Some c] such that [c] is unsatisfiable.
   see frontend test [Cstr.isContrad c] (of CstrC.v)

NB: in the current implementation, all generated polyhedra are non-empty
(and redundant constraints are removed).

Hence, no actual test is needed !
*)
let isEmpty: ('c pedraCert) -> ('c option)
= fun _ -> None

(* [isIncl (p1,p2)]
 - returns [None]  (if [p1] is not included in [p2])
 - or, returns [Some (is_triv,l)] such that
    if [is_triv]
    then [l] is a singleton [c] such that [c] is unsatisfiable (see [isEmpty] above)
    else [l] is "syntactically" equals to [p2.cert]
    => each constraints of [l] must exactly match the corresponding one in [p2.cert]
    (see frontend test [Cstr.isEq] of CstrC.v)

NB: in the current implementation, case [is_triv=true] can not happen !
*)
let isIncl: ('c pedraCert) * t -> (bool * ('c list)) option
= function (p1, p2) ->
  let lcf = cstrLCF_from_frontend p1.lcf in
  let ip1 = import lcf p1.backend p1.cert in
  match Pol.incl lcf ip1 p2 with
  | Pol.NoIncl -> None
  | Pol.Incl cert -> Some (false, List.rev_append cert [])

(* [add p1 c]
   - assumes that [p1.cert] corresponds to the list of certificates in
                  [p1.backend] which has been appended to [c]
     ([c] is thus the last constraint of [p1.cert]
      but it has not yet been inserted in [p1.backend])
   - returns [(None,c')] such that [c'] is unsatisfiable (see [isEmpty] above).
     Hence, [p1.backend /\ c] is empty
   - or returns [(Some p',l)] such that
   [p'] is the backend representation for [p1.backend /\ c]
   and [l] is the list of certificates corresponding to [p']
   NB: there is no test in the frontend for this last case !
*)
let add: ('c pedraCert) * ('c list) -> (t option) * ('c list)
= function (p, fc) ->
  let lcf = cstrLCF_from_frontend p.lcf in
  let ip = import lcf p.backend p.cert in
  let ic = direct_import p.lcf fc in
  match Pol.addM lcf ip ic with
  | Pol.Added p0 ->
     let (p', ce) = export p0 in
     (Some p', ce)
  | Pol.Contrad ce ->
     (None, [ce]);;

let meet: ('c pedraCert) * (t * 'c list) -> (t option) * ('c list)
= function (p1, (p2, cert2)) ->
  let lcf = cstrLCF_from_frontend p1.lcf in
  let ip1 = import lcf p1.backend p1.cert in
  let ip2 = import lcf p2 cert2 in
  match Pol.meet lcf ip1 ip2 with
  | Pol.Added p0 ->
     let (p', ce) = export p0 in
     (Some p', ce)
  | Pol.Contrad ce ->
     (None, [ce])

(* [join ((p1,p2),unify)] returns [(p,l)] *)
let join: (('c1 pedraCert) * ('c2 pedraCert)) * ('c1 -> 'c2 -> 'c3) -> (t * ('c3 list))
= fun ((p1, p2), unify) ->
  let lcf1 = cstrLCF_from_frontend p1.lcf in
  let lcf2 = cstrLCF_from_frontend p2.lcf in
  let ip1 =  import lcf1 p1.backend p1.cert in
  let ip2 =  import lcf2 p2.backend p2.cert in
  let (p1, p2) = Pol.join lcf1 lcf2 ip1 ip2 in
  join_export unify p1 p2

  
(* [project p x] returns [(p,l)]
 such that [p] contains [p] but has no occurrence of [x]
 and such that [l] is a list of certificates corresponding to [p].
 The frontend tests [x] is free in [l]:
   it applies [Cstr.isFree] of CstrC.v for each constraint.
*)
let project: ('c pedraCert) * ProgVar.PVar.t -> t * ('c list)
= fun (p, x) ->
  let lcf = cstrLCF_from_frontend p.lcf in
  let ip = import lcf p.backend p.cert in
  let x' = progVarToVar x in
  export (Pol.project lcf ip x')

(* [rename ((x,y),p1)]
   - assumes that [y] is a variable fresh in p1
     (otherwise, this is a bug of the front-end or from vpl clients)
   - returns [p1] where [x] has been renamed in [x]
*)
let rename: (ProgVar.PVar.t * ProgVar.PVar.t) * t -> t
= fun ((x,y),p) -> Pol.rename trivLCF (progVarToVar x) (progVarToVar y) p

(* printing function for the frontend *)
let pr: t -> char list
= fun p -> CoqPr.stringTr (Pol.to_string_raw p)

let widen: t * t -> t * ConsSet.Cs.t
= unimplemented "widen"
(*
= fun (p1,p2) ->
	let p = Pol.widen p1 p2 in
	(p, pToConsSet p)
*)

let bToBnd: Pol.bndT -> 'c option -> 'c CstrLCF.bndT
= fun b opt ->
  match b with
  | Pol.Infty -> CstrLCF.Infty
  | Pol.Open n ->
    (match opt with
    | None -> assert false
    | Some c ->
      CstrLCF.Open (nToNumC n,c))
  | Pol.Closed n ->
    (match opt with
    | None -> assert false
    | Some c ->
      CstrLCF.Closed (nToNumC n,c))

let getItv: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.itvT
= fun (p,lt) ->
  let lt = ltToVec lt in
  (* Printf.printf "getItv %s\n" (Vec.to_string Var.to_string lt); *)
  let lcf = cstrLCF_from_frontend p.lcf in
  let ip = import lcf p.backend p.cert in
  let (itv, lpf, upf) = Pol.itvize lcf ip lt in
  {
    CstrLCF.low = bToBnd (Pol.get_low itv) lpf;
    CstrLCF.up = bToBnd (Pol.get_up itv) upf
  }

let getUpperBound: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.bndT
= fun (p, lt) ->
  let lt = ltToVec lt in
  (* Printf.printf "getUpperBound %s\n" (Vec.to_string Var.to_string lt); *)
  let lcf = cstrLCF_from_frontend p.lcf in
  let ip = import lcf p.backend p.cert in
  let (b, pf) = Pol.getUpperBound lcf ip lt in
  bToBnd b pf

let getLowerBound: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.bndT
= fun (p, lt) ->
  let lt = ltToVec lt in
  (* Printf.printf "getLowerBound %s\n" (Vec.to_string Var.to_string lt); *)
  let lcf = cstrLCF_from_frontend p.lcf in
  let ip = import lcf p.backend p.cert in
  let (b, pf) = Pol.getLowerBound lcf ip lt in
  bToBnd b pf

let export_backend_rep (p, (a, u)) =
  (p,
   (fun x -> progVarToVar (a (varToProgVar x))),
   (fun x -> progVarToVar (u (varToProgVar x))))
