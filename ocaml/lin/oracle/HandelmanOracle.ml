(** Handelman oracle. *)

open OraclePattern
open HOtypes

module Debug = OraclePattern.Debug

type prophecy_ = {
    vl : Var.t list; (** List of variables in the problem *)
    mapP : MapPolyHi.t; (** Map from polynomials to His *)
    mapIP : MapIndexP.t; (** Map from indexes to polynomials. *)
    mapI : IndexBuild.Map.t; (** Map that decomposes indexes *)
    ph : Poly.t list; (** Constaints of the input polyhedron. *)
    sx : Splx.t; (** A simplex tableau containing constraints of the polyhedron. It is used to determine constant bounds. *)
    lp : LPMaps.bounds (** Known bounds for each variable. *)
}

let hi_to_poly : Hi.t -> prophecy_ -> Poly.t * MapIndexP.t * IndexBuild.Map.t
    = fun hi pn ->
    match hi with
    | Hi.Ci i -> MapIndexP.get i pn.mapIP pn.mapI
    | Hi.VarCi (j,i) ->
        let (pi,mapIP',mapI') = MapIndexP.get i pn.mapIP pn.mapI in
        (Poly.mul pi (Hi.computeVarIndex j pn.vl), mapIP', mapI')
    | Hi.VarBounds (j,b) -> (Poly.mul (Hi.computeBoundIndexList b pn.ph) (Hi.computeVarIndex j pn.vl), pn.mapIP, pn.mapI)

module OracleCore = struct

    type prophecy = prophecy_

    module P = Poly
    module M = P.Monomial
    module MB = P.MonomialBasis

    module type Prayer = sig
        val name : string
        type pneuma
        val kill_when_fail : bool
        val pray : P.t -> prophecy -> pneuma option
        val inhale : P.t -> prophecy -> pneuma -> P.t * prophecy
    end

    let recursive_oracle : (P.t -> prophecy -> (module Prayer) list -> prophecy) ref
        = ref (fun _ pr _ -> pr)

    let prophecy_to_string : prophecy -> string
        = fun pr ->
    		Printf.sprintf "\n\tVariables : %s\n\tMap Poly -> Index list :\n%s \n\tMap Index -> Poly :\n%s\n\tMap Index -> Index list : \n%s"
    		(Poly.MonomialBasis.to_string (Poly.MonomialBasis.mk_expanded pr.vl))
    		(MapPolyHi.to_string pr.mapP |> Misc.add_tab 2)
    		(MapIndexP.to_string pr.mapIP |> Misc.add_tab 2)
    		(IndexBuild.Map.to_string pr.mapI |> Misc.add_tab 2)

    (** @return the number of constraints of the polyhedron. *)
    let n_cstrs : prophecy -> int
		= fun pr ->
		List.length pr.ph

    (** We already know how to cancel a monomial. *)
    module rec AlreadyIn : Prayer = struct
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
    and LinearMonomial : Prayer = struct
        type pneuma = M.t

        let name = "Linear Monomial"

        let kill_when_fail = false

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

    and ExtractEvenPowers : Prayer = struct

        type pneuma = M.t * Index.Int.t

        let name = "Extract even powers"

        let kill_when_fail = false

        let rec pray p pr =
            match p with
            | [] -> None
            | (mb,_) as m :: p' ->
                if MapPolyHi.memMonom mb pr.mapP
                then pray p' pr
                    else let id = MapIndexP.poly_to_deg_max [m] pr.vl in
        			if Misc.max Stdlib.compare (Index.Int.data id) > 1
        			then Some (m, id)
        			else pray p' pr

        (** @return the greatest multiple of 2 smaller than [i].
            @param i the bound *)
        let rec get_max_power_two : int -> int
            = fun i ->
            if i <= 1
            then 0
            else 2 + get_max_power_two (i-2)

        (** @return an index of even powers
            @return the next index to find *)
        let extract : Index.Int.t -> (Index.Int.t * Index.Int.t)
			= fun id ->
			let (id1,id2) = List.map
				(fun i -> (get_max_power_two i, i mod 2))
				(Index.Int.data id)
			|> List.split
			|> fun (i1,i2) -> (Index.Int.mk i1, Index.Int.mk i2)
			in
            if Index.Int.is_null id2
			then let j = Index.Int.first_positive id1 in
				let id1' = Index.Int.set id1 j ((Index.Int.get id1 j)-2) in
				let id2' = Index.Int.set id2 j 2 in
				(id1',id2')
			else (id1,id2)

        let compute_new_poly : P.t list -> P.t -> M.t -> Scalar.Rat.t -> P.t
			= fun pl poly mon coeff ->
			let pl = List.fold_left (fun l p ->
                try
					let (_,c') = List.find (fun m' ->
                            M.compare m' mon = 0
                        ) p
					in
					(p,c') :: l
				with Not_found -> l)
				[] pl
			in
			let n = List.length pl in
            List.map (fun (p,c') ->
                Q.div coeff (Q.mul (Q.of_int n) c')
                |> Poly.cste
                |> Poly.mul p
            ) pl
            |> P.sum
            |> P.sub poly

        let rec updateHi : Hi.t list -> Index.Int.t -> Hi.t list
			= fun hil id ->
			if Index.Int.is_null id
			then hil
			else match hil with
				| [] -> []
				| Hi.VarBounds (vI,bIl) :: l ->
                    Hi.VarBounds(Index.Int.add vI id, bIl) :: (updateHi l id)
				| Hi.Ci (ci) :: l ->
                    Hi.VarCi(id,ci) :: (updateHi l id)
				| _ -> Stdlib.failwith "HHeuristic.extractEvenPowers.updateHi"

        let inhale p pr ((mb,c),id) =
            let (id,idm') = extract id in
			let mb' = List.combine pr.vl idm'
                |> MB.mk
            in
			let p' = [mb', c] in
            let prayers_ltone : (module Prayer) list = [
                (module AlreadyIn);
                (module DegreeLTOne);
            ] in
			let pr' = !recursive_oracle p' pr prayers_ltone in
			let hil = updateHi (MapP.find p' pr'.mapP) id in
			Debug.log DebugTypes.Detail
				(lazy("new Hi= " ^ (Misc.list_to_string Hi.to_string hil " ; ")));
			let (hi_p,mapIP',mapI') = List.fold_right (fun hi (l,mapIP,mapI) ->
					let (p,mapIP',mapI') = hi_to_poly hi {pr' with
                        mapIP = mapIP;
                        mapI = mapI
                    } in
					(p :: l,mapIP',mapI')
				) hil ([],pr.mapIP, pr.mapI)
			in
			let final_p = compute_new_poly hi_p p (mb,c) c in
			let mapP' = MapPolyHi.merge pr.mapP pr'.mapP
                |> MapP.add [(mb,c)] hil
            in
            let final_pr = {pr' with
                mapP = mapP';
                mapIP = mapIP';
                mapI = mapI'
            } in
            (final_p, final_pr)
    end

    and DegreeLTOne : Prayer = struct

        type pneuma = M.t

        let name = "Variables have degree <= 1"

        let kill_when_fail = false

        let pray p pr =
            let one = List.map (fun _ -> 1) pr.vl |> Index.Int.mk in
            let rec pray p pr =
                match p with
                | [] -> None
                | m :: p' ->
                    let id = MapIndexP.poly_to_deg_max (Poly.mk [m]) pr.vl in
                    if Index.Int.le id one
                    then Some m
                    else pray p' pr
            in
            pray p pr

        let get_coeff : P.t -> M.t -> Q.t
            = fun p (mb,c) ->
            let (_,c') = List.find (fun (mb',_) ->
                MB.equal mb mb'
                ) p in
            Q.mul Q.minus_one (Q.div c c')

        let inhale p pr m =
            let (bI,bounds) = match HLP.run pr.lp pr.ph pr.sx pr.vl m with
                | (Some bI, bounds) -> (bI,bounds)
                | (None,_) -> Stdlib.raise Not_found
            in
            Debug.log DebugTypes.Detail (lazy(
                Printf.sprintf "Result LP = %s"
                (Misc.list_to_string Index.Rat.to_string bI " ; ")));
            let id = List.length pr.vl |> Index.Int.init in
            let newHi = Hi.VarBounds(id,bI) in
            Debug.log DebugTypes.Detail
                (lazy("new Hi= " ^ (Hi.to_string newHi)));
            let (p',mapIP',mapI') = hi_to_poly newHi pr in
            Debug.log DebugTypes.Detail
                (lazy(Printf.sprintf "Corresponding polynomial: %s" (P.to_string p')));
            let mapP' = MapP.add (Poly.mk [m]) [newHi] pr.mapP in
            let p'' = get_coeff p' m
                |> Poly.mulc  p'
                |> Poly.add p
            in
            Debug.log DebugTypes.Detail
                (lazy("p''= " ^ (Poly.to_string p'')));
            let pr' = {pr with
                mapP = mapP';
                mapIP = mapIP';
                mapI = mapI';
                lp = bounds;
            } in
            (p'', pr')

    end

    let prayers : (module Prayer) list = [
        (module AlreadyIn);
        (module LinearMonomial);
        (module ExtractEvenPowers);
        (module DegreeLTOne);
    ]
end

include Make(OracleCore)

(** Initializes a prophecy.
    @param p the polynomial to linearize (ie. to cancel)
    @param ph the input polyhedron
    @return an initial prophecy
    @return the polynomial to build (ie. [-1*p]) *)
let init : P.t -> 'c HPol.t -> P.t * prophecy
	= fun p ph ->
	let cl = HPol.get_noneq_poly ph
        |> List.map neg_poly
        |> List.concat
	in
	let inequalities = HPol.get_ineqs ph in
	let sx = Splx.mk
		(HPol.horizon ph)
		(List.mapi (fun i cstr -> (i,cstr)) inequalities)
	in
	match sx with
	| Splx.IsOk sx ->
        let pr : prophecy = {
            vl = ph.vars;
            mapIP = MapIndexP.of_polyhedron cl;
            mapP = MapP.empty;
            mapI = IndexBuild.MapI.empty;
            ph = cl;
            sx = sx;
            lp = LPMaps.init cl ph.vars;
        } in
        (P.neg p, pr)
	| Splx.IsUnsat _ -> Stdlib.failwith "Handelamn Oracle init : empty polyhedron"

let oracle_hi: Poly.t -> 'c HPol.t -> Hi.t list * Poly.t list
	= fun p ph ->
	Debug.log DebugTypes.Title (lazy "Handelman Oracle");
	Debug.log DebugTypes.MInput
		(lazy (Printf.sprintf "Polynomial %s\nPolyhedron %s"
		(Poly.to_string p)
		(HPol.to_string ph)));
    let (p', init_prophecy) = init p ph in
	let pr = run init_prophecy p' in
    let his = MapP.bindings pr.mapP
		|> List.map (fun (_,i) -> i)
		|> List.concat
		|> Misc.rem_dupl Hi.eq
		|> List.filter (fun hi -> Hi.is_linear hi |> not)
    in
    let polys = List.map (fun hi ->
        let (p,_,_) = hi_to_poly hi pr in p
    ) his
    in
	Debug.exec (his,polys)
		DebugTypes.MOutput (lazy (Printf.sprintf "His = %s\nPolys associated = %s"
			(Misc.list_to_string Hi.to_string his " ; ")
			(Misc.list_to_string Poly.to_string polys " ; ")))
