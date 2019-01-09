(** Handelman oracle. *)

open OraclePattern

module Oracle = struct
    open HOtypes

    type prophecy = {
    	vl : Var.t list; (** List of variables in the problem *)
    	mapP : MapPolyHi.t; (** Map from polynomials to His *)
    	mapIP : MapIndexP.t; (** Map from indexes to polynomials. *)
    	mapI : IndexBuild.Map.t; (** Map that decomposes indexes *)
    	ph : Poly.t list; (** Constaints of the input polyhedron. *)
    	sx : Splx.t;
    	lp : LPMaps.bounds
    }

    let prophecy_to_string : prophecy -> string
        = fun pr ->
    		Printf.sprintf "\n\tVariables : %s\n\tMap Poly -> Index list :\n%s \n\tMap Index -> Poly :\n%s\n\tMap Index -> Index list : \n%s"
    		(Poly.MonomialBasis.to_string (Poly.MonomialBasis.mk_expanded pr.vl))
    		(MapPolyHi.to_string pr.mapP |> Misc.add_tab 2)
    		(MapIndexP.to_string pr.mapIP |> Misc.add_tab 2)
    		(IndexBuild.Map.to_string pr.mapI |> Misc.add_tab 2)

    (** Converts a polynomial constraint into a list of polynomials.
        The oracle treats polynomials of the form [p >= 0]. *)
	let neg_poly : CstrPoly.t -> Poly.t list
		= fun cp ->
		let p = cp.CstrPoly.p in
		match cp.CstrPoly.typ with
		| Cstr_type.Le -> [Poly.neg p]
		| Cstr_type.Lt -> [Poly.neg p]
		| Cstr_type.Eq -> p :: [Poly.neg p]
end
