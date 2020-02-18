type cmpT = Cstr_type.cmpT_extended

module Cs = Cstr.Rat
module Vec = Cs.Vec
module CP = CstrPoly
module Polynomial = CP.Poly
module Coeff = Scalar.Rat

module CW = CWrappers

(**
 * This functor builds a {!module-type:NCInterface.PolyhedronDomain} by instantiating operators in {!module:Pol} with the given factory.
 *)
module MakePolyhedronDomain (FM : FactoryMaker.Type) = struct

    module F = FactoryMaker.Make(FM)

    type cert = F.cert

	type t =
		| NonBot of cert Pol.t
		| Bottom of cert

	exception Wrong_Certificate of string

	let top = NonBot Pol.top

	let bottom = Bottom (F.mk (Cs.mk Cstr_type.Le [] Scalar.Rat.z))

	let is_bottom = function
        | Bottom _ -> true
        | _ -> false

    let get_bottom_cert = function
        | Bottom c -> Some c
        | _ -> None

	let to_string : (Var.t -> string) -> t -> string
		= fun varPr -> function
		| Bottom _ -> "bottom"
		| NonBot p ->
			if p = Pol.top then
				"top"
			else
				Pol.to_string_ext F.factory varPr p

	let check : t -> t
		= function
		| Bottom c -> Bottom c (* XXX : test the certificate *)
		| NonBot p as ph -> if F.check p
			then ph
			else Stdlib.failwith (Printf.sprintf "VPL has failed: wrong certificate in polyhedron %s"
				(to_string Var.to_string ph))

	let addM : t -> Cs.t list -> t
		= fun p cs ->
		match p with
		| Bottom _ -> p
		| NonBot p ->
			let cs' = List.map F.mkCons cs in
			match Pol.addM F.factory p cs' with
			| Pol.Added pol -> check (NonBot pol)
			| Pol.Contrad c -> Bottom c

	let addNLM : t -> CP.t list -> t
		= fun _ _ ->
		Stdlib.failwith "VPL.addNLM: unimplemented"

    let assume_back : t -> Cs.t -> t option
		= fun p cs ->
		match p with
		| Bottom _ -> Some p
		| NonBot p ->
			let cs' = F.mkCons cs in
            match Pol.assume_back F.factory p cs' with
            | Some p -> Some (NonBot p)
            | None -> None

	let meet : t -> t -> t
		= fun p1 p2 ->
		match p1,p2 with
		| Bottom c, _ | _, Bottom c -> Bottom c
		| NonBot p1', NonBot p2' ->
			match Pol.meet F.factory p1' p2' with
			| Pol.Added pol -> check (NonBot pol)
			| Pol.Contrad c -> Bottom c

	(* TODO: need Factory.equal! *)
	let join : t -> t -> t
		= fun p1 p2 ->
		match p1, p2 with
		| Bottom c, Bottom _ -> Bottom c
		| Bottom _, NonBot _ -> p2
		| NonBot _, Bottom _ -> p1
		| NonBot p1', NonBot p2' ->
			let p' = Pol.join F.factory F.factory p1' p2'
				|> Stdlib.fst
			in
			check (NonBot p')

	(* TODO: need Factory.equal! *)
	let minkowski : t -> t -> t
		= fun p1 p2 ->
		match p1, p2 with
		| Bottom c, Bottom _ -> Bottom c
		| Bottom _, NonBot _ -> p2
		| NonBot _, Bottom _ -> p1
		| NonBot p1', NonBot p2' ->
			let p' = Pol.minkowski F.factory F.factory p1' p2'
				|> Stdlib.fst
			in
			check (NonBot p')

	let project : t -> Var.t list -> t
		= fun p vars ->
		match p with
		| Bottom c -> Bottom c
		| NonBot p' ->
			let p' = Pol.project F.factory p' vars in
			check (NonBot p')

	let widen: t -> t -> t
		= fun p1 p2 ->
		match p1, p2 with
		| Bottom c, Bottom _ -> Bottom c
		| Bottom _, NonBot _ -> p2
		| NonBot _, Bottom _ -> top
		| NonBot p1', NonBot p2' ->
			let p' = Pol.widen F.factory p1' p2' in
			check (NonBot p')

	(* TODO: lever une exception spécifique*)
	let check_incl : cert list -> t -> unit
		= fun rel -> function
		| Bottom _ -> ()
		| NonBot p ->
			if not (Misc.list_eq2 F.equal (Pol.get_cstr p) rel)
			then Stdlib.failwith "VPL has failed: wrong certificate in inclusion"

	let incl : t -> t -> bool
		= fun p1 p2 ->
		match p1,p2 with
		| Bottom _, Bottom _ | Bottom _, NonBot _ -> true
		| NonBot _, Bottom _ -> false
		| NonBot p1', NonBot p2' ->
			match Pol.incl F.factory p1' p2' with
			| Pol.Incl cert -> (check_incl cert (NonBot p2');true)
			| Pol.NoIncl -> false

	let check_bound : bool -> Vec.t -> Pol.bndT * cert option -> Pol.bndT
		= fun upper obj (bnd,cert) ->
        let error_string = if upper
            then "check_upper_bound"
            else "check_lower_bound"
        in
		match (bnd,cert) with
		| (Pol.Infty, None) -> Pol.Infty
		| (_, None)
		| (Pol.Infty, Some _) -> Stdlib.raise (Wrong_Certificate error_string)
		| (Pol.Open v, Some cert) ->
            let expected_cert = if upper
                then (Cs.mk2 Cstr_type.Lt obj v)
                else (Cs.mk2 Cstr_type.Lt (Vec.neg obj) (Scalar.Rat.neg v))
            in
			if F.equal expected_cert cert
			then Pol.Open v
			else Stdlib.raise (Wrong_Certificate
                (Printf.sprintf "%s -> cstr: %s; expected %s, got: %s"
                    error_string
                    (Pol.bnd_to_string bnd)
                    (Cs.to_string Var.to_string expected_cert)
                    (F.to_string cert)))
		| (Pol.Closed v, Some cert) ->
			let expected_cert = if upper
                then (Cs.mk2 Cstr_type.Le obj v)
                else (Cs.mk2 Cstr_type.Le (Vec.neg obj) (Scalar.Rat.neg v))
            in
			if F.equal expected_cert cert
			then Pol.Closed v
			else Stdlib.raise (Wrong_Certificate
                (Printf.sprintf "%s -> cstr: %s; expected %s, got: %s"
                    error_string
                    (Pol.bnd_to_string bnd)
                    (Cs.to_string Var.to_string expected_cert)
                    (F.to_string cert)))

	let getUpperBound : t -> Vec.t -> Pol.bndT option
		= fun p vec ->
		match p with
		| Bottom _ -> None
		| NonBot p' -> Some (Pol.getUpperBound F.factory p' vec |> check_bound true vec)

	let getLowerBound : t -> Vec.t -> Pol.bndT option
		= fun p vec ->
		match p with
		| Bottom _ -> None
		| NonBot p' -> Some (Pol.getLowerBound F.factory p' vec |> check_bound false vec)

	let itvize : t -> Vec.t -> Pol.itvT
		= fun p vec ->
		match p with
		| Bottom _ -> {Pol.low = Pol.Closed Scalar.Rat.u ; Pol.up = Pol.Closed Scalar.Rat.z}
		| NonBot p' ->
			let (itv, certLower, certUpper) = Pol.itvize F.factory p' vec in
			let itvLower = check_bound false vec (itv.Pol.low, certLower)
			and itvUpper = check_bound true vec (itv.Pol.up, certUpper) in
			{Pol.low = itvLower ; Pol.up = itvUpper}

	let get_cstr = function
		| Bottom _ -> []
		| NonBot p -> Pol.get_cstr p

	let rename : Var.t -> Var.t -> t -> t
		= fun fromX toY -> function
		| Bottom c -> Bottom c
		| NonBot p ->
			NonBot (Pol.rename F.factory fromX toY p)

	type rep = unit Pol.t

  	let backend_rep : t -> rep option
  		= fun p ->
  		match p with
  		| Bottom _ -> None
  		| NonBot p ->
  			let eqs = List.map (fun (v, (c,_)) -> (v, (c,()))) p.Pol.eqs
  			and ineqs = IneqSet.map (fun _ -> ()) p.Pol.ineqs
  			in Some {Pol.eqs = eqs ; Pol.ineqs = ineqs; Pol.point = p.Pol.point}

    let get_regions : t -> t list
        = function
        | Bottom _ -> []
        | NonBot p ->
            Pol.get_regions F.factory p
            |> List.map (fun p -> NonBot (F.convert p))

    let set_point : Vec.t -> t -> t
        = fun point -> function
        | Bottom _ as p -> p
        | NonBot p -> NonBot (Pol.set_point point p)

    let proj_incl : t -> t -> t option
        = fun p1 p2 ->
        match p1,p2 with
        | _, Bottom _ -> None
        | Bottom _, _ -> Some p1
        | NonBot p1, NonBot p2 ->
            match Pol.proj_incl F.factory p1 p2 with
            | None -> None
            | Some p -> Some (NonBot p)

end

let translate_cstr : Cs.t -> Vec.t -> Cs.t
	= fun cstr vec ->
	let v = Cs.get_v cstr in
	let l = Vec.toList v in
	let (var,coeff) = List.hd l in
	let m = Scalar.Rat.div (Cs.get_c cstr) coeff
		|> Vec.set Vec.nil var
	in
	let m' = Vec.add vec m in
	let cste = Vec.dot_product m' v in
	{cstr with Cs.c = cste}

(** High level domain with ocaml verification of certificates. *)
module NCVPL_Cstr = struct
	module P = struct
		include MakePolyhedronDomain (FactoryMaker.Cstr)

		(** Careful : addNLM is UNcertified. *)
		let addNLM : t -> CP.t list -> t
			= fun p cps ->
			match p with
			| Bottom _ -> bottom
			| NonBot pol -> match Lin.addPolyM F.factory pol cps with
				| None -> bottom
				| Some pol' -> NonBot (F.convert pol')

	end
	module I = NCInterface.Lift (P)
	module I_Q = I.QInterface
	module Q = CW.MakeHighLevel (I_Q)

	module I_Z = I.ZInterface
	module Z = CW.MakeZ (I_Z)
end

(** High level domain with NO certificates. *)
module NCVPL_Unit = struct
	module P = struct
		include MakePolyhedronDomain (FactoryMaker.Unit)

		let addNLM : t -> CP.t list -> t
			= fun p cps ->
			match p with
			| Bottom _ -> bottom
			| NonBot pol -> match Lin.addPolyM F.factory  pol cps with
				| None -> bottom
				| Some pol' -> NonBot pol'

	end
	module I = NCInterface.Lift (P)
	module I_Q = I.QInterface
	module Q = CW.MakeHighLevel (I_Q)

	module I_Z = I.ZInterface
	module Z = CW.MakeZ (I_Z)
end
