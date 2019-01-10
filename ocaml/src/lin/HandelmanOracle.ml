(** Handelman oracle. *)

open OraclePattern

module OracleCore = struct
    open HOtypes

    module P = Poly
    module M = P.Monomial
    module MB = P.MonomialBasis

    type prophecy = {
    	vl : Var.t list; (** List of variables in the problem *)
    	mapP : MapPolyHi.t; (** Map from polynomials to His *)
    	mapIP : MapIndexP.t; (** Map from indexes to polynomials. *)
    	mapI : IndexBuild.Map.t; (** Map that decomposes indexes *)
    	ph : P.t list; (** Constaints of the input polyhedron. *)
    	sx : Splx.t;
    	lp : LPMaps.bounds
    }

    module type Prayer = sig
        val name : string
        type pneuma
        val kill_when_fail : bool
        val pray : P.t -> prophecy -> pneuma option
        val inhale : P.t -> prophecy -> pneuma -> P.t * prophecy
    end

    let prophecy_to_string : prophecy -> string
        = fun pr ->
    		Printf.sprintf "\n\tVariables : %s\n\tMap Poly -> Index list :\n%s \n\tMap Index -> Poly :\n%s\n\tMap Index -> Index list : \n%s"
    		(Poly.MonomialBasis.to_string (Poly.MonomialBasis.mk_expanded pr.vl))
    		(MapPolyHi.to_string pr.mapP |> Misc.add_tab 2)
    		(MapIndexP.to_string pr.mapIP |> Misc.add_tab 2)
    		(IndexBuild.Map.to_string pr.mapI |> Misc.add_tab 2)

    (** Converts a polynomial constraint into a list of polynomials.
        The oracle treats polynomials of the form [p >= 0]. *)
	let neg_poly : CstrPoly.t -> P.t list
		= fun cp ->
		let p = cp.CstrPoly.p in
		match cp.CstrPoly.typ with
		| Cstr_type.Le -> [Poly.neg p]
		| Cstr_type.Lt -> [Poly.neg p]
		| Cstr_type.Eq -> p :: [Poly.neg p]

    let init : P.t -> 'c HPol2.t -> P.t * prophecy
		= fun p ph ->
		let cl = HPol2.get_noneq_poly ph
            |> List.map neg_poly
            |> List.concat
		in
		let inequalities = HPol2.get_ineqs ph in
		let sx = Splx.mk
			(HPol2.horizon ph)
			(List.mapi (fun i cstr -> (i,cstr)) inequalities)
		in
		let p' = P.neg p in
		match sx with
		| Splx.IsOk sx ->
            let pr = {
                vl = ph.vars;
                mapIP = MapIndexP.of_polyhedron cl;
                mapP = MapP.empty;
                mapI = IndexBuild.MapI.empty;
                ph = cl;
                sx = sx;
                lp = LPMaps.init cl ph.vars;
            } in
            (p', pr)
		| Splx.IsUnsat _ -> Pervasives.failwith "HOtypes.Pneuma.init : polyhedron empty"

    let n_cstrs : prophecy -> int
		= fun pr ->
		List.length pr.ph

    let computeVarIndex : Index.Int.t -> Var.t list -> P.t
		= fun id vl ->
		let varlist = List.fold_left2 (fun r i v ->
            r @ (List.map (fun _ -> v) (Misc.range 0 i))
        ) [] (Index.Int.data id) vl
		in
		Poly.mk_expanded_list [varlist, Q.of_int 1]

    let computeBoundIndex : Index.Rat.t -> P.t list -> P.t
		= fun id polyhedron ->
		List.fold_left2 (fun p ci q ->
            P.add p (Poly.mul (Poly.cste q) ci)
        ) P.z polyhedron (Index.Rat.data id)

    let computeBoundIndexList : Index.Rat.t list -> P.t list -> P.t
		= fun il polyhedron ->
		List.map (fun i ->
            computeBoundIndex i polyhedron
        ) il
        |> P.prod

    let hi_to_poly : Hi.t -> prophecy -> P.t * MapIndexP.t * IndexBuild.Map.t
		= fun hi pn ->
		match hi with
		| Hi.Ci i -> MapIndexP.get i pn.mapIP pn.mapI
		| Hi.VarCi (j,i) -> let (pi,mapIP',mapI') = MapIndexP.get i pn.mapIP pn.mapI in
			(Poly.mul pi (computeVarIndex j pn.vl), mapIP', mapI')
		| Hi.VarBounds (j,b) -> (Poly.mul (computeBoundIndexList b pn.ph) (computeVarIndex j pn.vl), pn.mapIP, pn.mapI)

    module AlreadyIn : Prayer = struct
        type pneuma = M.t

        let name = "Already In"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | m :: p' -> if MapPolyHi.memMonomSigned m pr.mapP
                then Some m
                else pray p' pr

        let inhale p pr m =
            let p' = P.sub p [m] in
            (p', pr)

    end

    module LinearMonomial : Prayer = struct
        type pneuma = M.t

        let name = "Linear Monomial"

        let kill_when_fail = true

        let rec pray p pr =
            match p with
            | [] -> None
            | m :: p' ->
                if M.is_linear m
                then Some m
                else pray p' pr

        let inhale p pr m =
            let p' = P.sub p [m] in
            (p', pr)
    end

    let prayers : (module Prayer) list = [
        (module AlreadyIn);
        (module LinearMonomial);
    ]
end

module Oracle = struct
    include Make(OracleCore)
end
