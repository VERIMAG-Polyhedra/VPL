open WrapperTraductors

(*************************************************************)
(*                  LowLevel -> HighLevel                    *)
(*************************************************************)
module MakeHighLevel (LHD: QInterface.LowLevelDomain) : QInterface.HighLevelDomain with type rep = LHD.rep = struct

  open DomainInterfaces

  module QAtomicCond = ASAtomicCond.QAtomicCond

  module DomainITV = PedraQIntervalization.Lift(LHD)

  module FullDom = DomainFunctors.MakeFull(CQNum)(CQCond)(QItv)(QAtomicCond)(DomainITV)(DomainITV)(DomainITV)(DomainITV)(DomainITV)

    (* BELOW = a copy-paste from PedraQWrapper *)
  let not_yet_implemented s =
    raise (CertcheckerConfig.CertCheckerFailure (Debugging.NYI, s ^ " on Q"))

  include FullDom

  type cert = LHD.cert

  module Term = QInterface.Term

  let auto_lifting : (LHD.t -> LHD.t) -> t -> t
    = fun f poly ->
    {poly with pol = f poly.pol}

  let auto_lifting2 : (LHD.t -> LHD.t -> LHD.t) -> t -> t -> t
    = fun f p1 p2 ->
    {p1 with pol = f p1.pol p2.pol}

  let proj_incl p1 p2 =
    match LHD.proj_incl p1.pol p2.pol with
    | Some p -> Some {p1 with pol = p}
    | None -> None

  let minkowski = auto_lifting2 LHD.minkowski

  let project_vars vars pol =
  match backend_rep pol with
  | None -> Stdlib.failwith "project_vars"
  | Some (p,(ofVar,toVar)) ->
    let (_,ofVar',_) = PedraQOracles.export_backend_rep (p,(ofVar,toVar)) in
    let vars' = List.map ofVar' vars in
  	{pol with pol = LHD.project vars' pol.pol}

  let set_point point p =
      let (_, toVar) = match backend_rep p with
          | Some (p',(ofVar,toVar)) ->
              let (_,_,toVar') = PedraQOracles.export_backend_rep (p',(ofVar,toVar)) in
              (p', toVar')
          | _ -> Stdlib.failwith "get_vars"
      in
      let point' = Vec.rename_f toVar point in
      auto_lifting (LHD.set_point point') p

  let get_regions p =
    List.map (fun p' -> {p with pol = p'}) (LHD.get_regions p.pol)

  let is_bottom = isBottom

  let get_bottom_cert p = LHD.get_bottom_cert p.pol

  let assume c p = assume (import_QCond c) p

  let assume_back c p =
      match backend_rep p with
      | None -> Stdlib.failwith "assume_back"
      | Some (pol,(ofVar,toVar)) ->
        let (_,ofVar',_) = PedraQOracles.export_backend_rep (pol,(ofVar,toVar)) in
        let rec to_term = function
            | QCond.Basic _ -> []
            | QCond.Atom (t1,c,t2) -> [c, QTerm.Add (QTerm.rename_f ofVar' t2, (QTerm.Opp (QTerm.rename_f ofVar' t1)))]
            | QCond.BinL (c1, AND, c2) -> to_term c1 @ to_term c2
            | _ -> invalid_arg "assume_back : to_term"
        in
        match LHD.assume_back (to_term c) p.pol with
        | Some pol -> Some {p with pol = pol}
        | None -> None

  let asserts c p =
    coq_assert (import_QCond c) p

  let rename x y = rename (import_certvar x) (import_certvar y)

  let assign l p =
    match l with
    | [] -> p
    | [(x, t)] -> FullDom.assign (import_certvar x) (import_QTerm t) p
    | _ -> not_yet_implemented "assign"

  let guassign l c p =
    match l with
    | [] -> p
    | [x] -> FullDom.guassign (import_certvar x) (import_QCond c) p
    | _ -> not_yet_implemented "guassign"

  let rec project l p =
    match l with
    | [] -> p
    | x::l -> project l (FullDom.project p (import_certvar x))

  let leq = isIncl

    (* REALLY INEFFICIENT. TO CHANGE ? *)
  let to_string f p =
    CoqPr.charListTr (to_string (fun x -> CoqPr.stringTr (f (export_certvar x))) p)

  let itvize p t =
    let itv = getItvMode BOTH (import_QTerm t) p in
    { Pol.low = export_QbndT itv.QItv.lower ; Pol.up = export_QbndT itv.QItv.upper }

  let getUpperBound p t =
    try Some (export_QbndT (getItvMode UP (import_QTerm t) p).QItv.upper)
    with Failure s when String.compare s "empty" = 0 -> None

  let getLowerBound p t =
    try Some (export_QbndT (getItvMode LOW (import_QTerm t) p).QItv.lower)
	 with Failure s when String.compare s "empty" = 0 -> None

end

(*************************************************************)
(*            LowLevel on Q -> HighLevel on Z                 *)
(*************************************************************)

module MakeZ (LHD: QLowLevelDomain) : ZInterface.HighLevelDomain with type rep = LHD.rep = struct

  open DomainInterfaces

  module DW = struct

    include LHD

    let isBottom = is_bottom

    let isIncl = leq

    let project p x =  project [export_certvar x] p

    let assume c = assume [(export_s_cmpT c.CQCstr.typ,
                            (PedraQOracles.ltToVec (LinTerm.LinQ.opp c.CQCstr.coefs),
                             export_Q c.CQCstr.cst))]

	let getItvMode mo t p =
		match mo with
		| BOTH ->
			let itv = itvize p (export_QAffTerm t) in
			{ QItv.lower = import_QbndT itv.Pol.low ;
			QItv.upper = import_QbndT itv.Pol.up }
		| UP -> begin
			match getUpperBound p (export_QAffTerm t) with
			| Some bound -> {
				QItv.lower = QItv.Infty ;
				QItv.upper = import_QbndT bound }
			| None -> failwith "empty"
			end
		| LOW -> begin
			match getLowerBound p (export_QAffTerm t) with
			| Some bound -> {
				QItv.lower = import_QbndT bound  ;
				QItv.upper = QItv.Infty }
			| None -> failwith "empty"
			end

    let rename x y = rename (export_certvar x) (export_certvar y)

    let pr p = CoqPr.stringTr (to_string Var.to_string p)

    let to_string f p = CoqPr.stringTr (to_string (fun x -> CoqPr.charListTr (f (import_certvar x))) p)

  end

  module FullDom = DomainFunctors.MakeZ(DW)(DW)(DW)(DW)(DW)

  include FullDom

  type cert = LHD.cert

  let not_yet_implemented s =
    raise (CertcheckerConfig.CertCheckerFailure (Debugging.NYI, "makeZ " ^ s ^ " on Z"))

  let auto_lifting : (LHD.t -> LHD.t) -> t -> t
    = fun f poly ->
    {poly with pol = f poly.pol}

  module Term = ZInterface.Term

  let is_bottom = FullDom.isBottom

  let get_bottom_cert p = LHD.get_bottom_cert p.pol

  let assume c p =
    assume (import_ZCond c) p

  let asserts c p =
    coq_assert (import_ZCond c) p

  let rename x y = rename (import_certvar x) (import_certvar y)

  let join = FullDom.join

  let assign l p =
    match l with
    | [] -> p
    | [(x, t)] -> FullDom.assign (import_certvar x) (import_ZTerm t) p
    | _ -> not_yet_implemented "assign"

  let guassign l c p =
    match l with
    | [] -> p
    | [x] -> FullDom.guassign (import_certvar x) (import_ZCond c) p
    | _ -> not_yet_implemented "guassign"


  let rec project l p =
    match l with
    | [] -> p
    | x::l -> project l (FullDom.project p (import_certvar x))

  let leq = FullDom.isIncl

    (* REALLY INEFFICIENT. TO CHANGE ? *)
  let to_string f p =
    CoqPr.charListTr (FullDom.to_string (fun x -> CoqPr.stringTr (f (export_certvar x))) p)

  let itvize p t =
    let itv = getItvMode BOTH (import_ZTerm t) p in
    { Pol.low = export_ZbndT itv.ZItv.low ; Pol.up = export_ZbndT itv.ZItv.up }

  let getUpperBound p t =
  	try Some (export_ZbndT (getItvMode UP (import_ZTerm t) p).ZItv.up)
  	with Failure s when String.compare s "empty" = 0 -> None

  let getLowerBound p t =
  	try Some (export_ZbndT (getItvMode LOW (import_ZTerm t) p).ZItv.low)
  	with Failure s when String.compare s "empty" = 0 -> None

  let minkowski p1 p2 =
    {p1 with pol = LHD.minkowski p1.pol p2.pol}

  let get_regions p =
    List.map (fun p' -> {p with pol = p'}) (LHD.get_regions p.pol)

  let set_point point pol = auto_lifting (LHD.set_point point) pol

  let project_vars vars pol =
  match backend_rep pol with
  | None -> Stdlib.failwith "project_vars"
  | Some (p,(ofVar,toVar)) ->
    let (_,ofVar',_) = PedraQOracles.export_backend_rep (p,(ofVar,toVar)) in
    let vars' = List.map ofVar' vars in
  	{pol with pol = LHD.project vars' pol.pol}

  let proj_incl _ _ = not_yet_implemented "proj_incl"

  let assume_back _ _ = not_yet_implemented "assume_back"

end
