open Vpl
open HOtypes

let x = Var.fromInt 1
let y = Var.fromInt 2
let z = Var.fromInt 3
let t = Var.fromInt 4

let mk = Index.Int.mk
let mkl l = List.map Index.Int.mk l

module MapIndexP = struct
	open MapIndexP

	let poly_to_deg_max_ts : Test.t
		= fun () ->
        let chk : string * (Poly.t * Var.t list) * Index.Int.t -> (Test.stateT -> Test.stateT)
		= fun (nm, (p,vl), ei) state ->
		let ai = poly_to_deg_max p vl in
		Test.equals nm Index.Int.to_string Index.Int.equal ei ai state
		   in
		   let tcs : (string * (Poly.t * Var.t list) * Index.Int.t ) list
		= [
			 "null polynomial", (Poly.z, [x;y;z]), mk [0;0;0] ;
			 "empty variable list", (Poly.of_string "x1 + 2*x2 + -1*x4", []), mk [] ;
			 "normal", (Poly.of_string "x1 + 2*x2*x1 + -1*x4*x3*x3", [x;y;z;t]), mk [1;1;2;1] ;
			 "non increasing variable order", (Poly.of_string "x1 + 2*x2*x1 + -1*x4*x3*x3", [x;z;t;y]), mk [1;2;1;1] ;
			 "one null variable", (Poly.of_string "x1 + 2*x1*x1 + -1*x4*x3*x3", [x;y;z;t]), mk [2;0;2;1] ;
		  ] in
		   Test.suite "poly_to_deg_max" (List.map chk tcs)

	module MB = Poly.MonomialBasis

	let ts : Test.t
		= fun () -> [
	     poly_to_deg_max_ts();
	  	] |> Test.suite "MapIndexP"
end

module LPMaps = struct
	open LPMaps

	let check_v_left : Var.t -> mapDetBound -> mapBound -> bool
		= fun v mapDB mapB ->
		let (opt,_) = MapV.find v mapDB in
		let (inf,_) = MapV.find v mapB in
		match (opt,inf) with
		| (Some true, Some bound) -> true (* XXX: vérifier aussi la valeur de la borne *)
		| (_,_) -> false

	let check_v_right : Var.t -> mapDetBound -> mapBound -> bool
		= fun v mapDB mapB ->
		let (_,opt) = MapV.find v mapDB in
		let (_,sup) = MapV.find v mapB in
		match (opt,sup) with
		| (Some true, Some bound) -> true (* XXX: vérifier aussi la valeur de la borne *)
		| (_,_) -> false

	module MB = Poly.MonomialBasis
	let check_maps : Poly.t list -> mapDetBound -> mapBound -> bool
		= fun pl mapDB mapB ->
		List.for_all
			(fun p ->
				match List.map Poly.Monomial.data (Poly.data p)
				with
				| [(m,c)] when Q.leq Q.zero c && (MB.to_list_expanded m |> List.length = 1) ->
					let v = List.hd (MB.to_list_expanded m) in
					check_v_left v mapDB mapB
				| (m0,_) :: [(m,c)] when Q.leq Q.zero c && (MB.to_list_expanded m |> List.length = 1)
					&& MB.equal m0 MB.null ->
					let v = List.hd (MB.to_list_expanded m) in
					check_v_left v mapDB mapB
				| [(m,c)] when Q.lt c Q.zero && (MB.to_list_expanded m |> List.length = 1) ->
					let v = List.hd (MB.to_list_expanded m) in
					check_v_right v mapDB mapB
				| (m0,_) :: [(m,c)] when Q.lt c Q.zero && (MB.to_list_expanded m |> List.length = 1)
					&& MB.equal m0 MB.null ->
					let v = List.hd (MB.to_list_expanded m) in
					check_v_right v mapDB mapB
				| _ -> true)
			pl

	let init_ts : Test.t
		= fun () ->
        let chk : string * (Poly.t list * Var.t list) -> (Test.stateT -> Test.stateT)
		= fun (nm, (pl,vl)) state ->
		let bounds = init pl vl in
		if check_maps pl bounds.mapDB bounds.mapB
			then Test.succeed state
			else Test.fail nm (Printf.sprintf "map check failed : from polynomials %s and variables %s\nmapDB : %s\nmapB : %s"
				(Misc.list_to_string Poly.to_string pl " ; ")
				(Misc.list_to_string Var.to_string vl ";")
				(mapDB_to_string bounds.mapDB)
				(mapB_to_string bounds.mapB))
				state
		   in
		   let tcs : (string * (Poly.t list * Var.t list)) list
		= [
			"empty polynomial list", ([], [x;y;z]) ;
			"empty variable list", ([Poly.of_string "x1 + 2*x1*x2"], []) ;
			"null polynomial", ([Poly.z], [x;y;z]) ;
			"normal", ([Poly.of_string "x1 + 2*x1*x2"], [x;y;z]) ;
		  ] in
		   Test.suite "init" (List.map chk tcs)

	let ts : Test.t
		= fun () -> [
	    init_ts()
	  	] |> Test.suite "LPMaps"
end

let ts : Test.t
		= fun () -> [
		 MapIndexP.ts() ;
		 LPMaps.ts()
	  	] |> Test.suite "HOtypes"
