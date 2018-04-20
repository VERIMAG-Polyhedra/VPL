open Vpl

module Cs = Cstr.Rat.Positive

module Test (Min : Min.Type) = struct
	
	module Vec = Min.Vec
	
	let x = Vec.V.fromInt 1
	let y = Vec.V.fromInt 2
	
	let minimizeTs: unit -> T.testT
		(* The boolean is true if the points can saturate constraints. *)
		= fun () ->
		let check_points: (string * (Cs.t * Vec.t) list * Cs.t list * bool) -> T.testT
			= fun (name, cstrs, _, b) state ->
			try 
				let (cstr,point) = List.find
					(fun (cstr,point) ->
						let point' = Vec.toRat point in
						let cstr_compl = Cs.compl cstr in
						not(Cs.satisfy point' cstr_compl)
					)
					cstrs
				in
				T.fail name
					(Printf.sprintf "Point %s, associated with constraint %s, does not satisfy compl(cstr)."
						(Vec.to_string Vec.V.to_string point)
						(Cs.to_string Cs.Vec.V.to_string cstr))
					state
			with Not_found -> 
			try
				let (cstr,point) = List.find
					(fun (cstr,point) ->
						let point' = Vec.toRat point in
						let cstr_compl = Cs.compl cstr in
						(not b) && Cs.saturate point' cstr_compl
					)
					cstrs
				in
				T.fail name
					(Printf.sprintf "Point %s, associated with constraint %s, saturates the constraint."
						(Vec.to_string Vec.V.to_string point)
						(Cs.to_string Cs.Vec.V.to_string cstr))
					state
			with Not_found -> 
			try
			 	let all_cstrs = List.split cstrs |> Pervasives.fst in
				let (cstr,point) = List.find
					(fun (cstr,point) ->
						let point' = Vec.toRat point in
						not (List.for_all 
						 		(fun cstr -> Cs.satisfy point' cstr) 
						 		(Misc.pop Cs.equalSyn all_cstrs cstr))
					)
					cstrs
				in
				T.fail name
					(Printf.sprintf "Point %s, associated with constraint %s, does not saturate other constraints."
						(Vec.to_string Vec.V.to_string point)
						(Cs.to_string Cs.Vec.V.to_string cstr))
					state	
			with Not_found -> T.succeed state	 	
		in
		let check_cstrs: (string * (Cs.t * Vec.t) list * Cs.t list * bool) -> T.testT 
			= fun (name, cstrs, ecstrs, _) state ->
			let acstrs = List.split cstrs |> Pervasives.fst in
			T.equals 
				name 
				(fun l -> Misc.list_to_string (Cs.to_string Cs.Vec.V.to_string) l " ; ")
				(Misc.list_eq (fun cstr cstrs -> List.exists (fun c -> Cs.equal c cstr) cstrs))
				ecstrs acstrs state
		in
		
		let tcs : (string * (Cs.t * Vec.t) list * Cs.t list * bool) list
			= [
					"singleton",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u],
					false
				;
					"interval",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2)],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2)],
					 false
				;
					"one_red",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					false
				;
					"one_red_1-epsilon",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.sub Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u],
					false
				;(* known to fail for glpk *)(*
					"one_red_1-epsilon_lt",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.sub Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u],
					false
				;*)
					"one_red_1+epsilon",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.add Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.add Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					false
				;
					"one_red_epsilon_lt",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.add Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.add Cs.Vec.Coeff.u Cs.Vec.Coeff.delta, x] Cs.Vec.Coeff.u],
					false
				;(* known to fail for glpk *)(*
					"one_red_epsilon/10_lt",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.add Cs.Vec.Coeff.u (Cs.Vec.Coeff.div Cs.Vec.Coeff.delta (Cs.Vec.Coeff.of_float 10.)), x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.add Cs.Vec.Coeff.u (Cs.Vec.Coeff.div Cs.Vec.Coeff.delta (Cs.Vec.Coeff.of_float 10.)), x] Cs.Vec.Coeff.u],
					false
				;*)(* known to fail *)(*
					"one_strict_red_swapped",
					Vec.nil,
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					false
				;*)
					"interval_one_red",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2);
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2)],
					 false
				;
					"interval_syntactic_red",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2);
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2)],
					 false
				;
					"interval_proportional_red",
					Vec.nil,
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 0.5, x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2);
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-5.), x] (Cs.Vec.Coeff.of_float 2.4);
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float 2., x] Cs.Vec.Coeff.u;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-2.5), x] (Cs.Vec.Coeff.of_float 1.2)],
					 false
				;
					"triangle_le",
					Vec.mk [Vec.Coeff.u, x ; Vec.Coeff.u, y],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.)],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.)],
					 false
				;
					"triangle_lt",
					Vec.mk [Vec.Coeff.u, x ; Vec.Coeff.u, y],
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.)],
					[Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.)],
					 false
				;
					"triangle_le_one_red",
					Vec.mk [Vec.Coeff.u, x ; Vec.Coeff.u, y],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.negU;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.negU;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.negU],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.negU;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.negU;],
					 false
				;
					"triangle_le_lt",
					Vec.mk [Vec.Coeff.u, x ; Vec.Coeff.u, y],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.z],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.z],
					 true
				;(*
					"triangle_le_lt2",
					Vec.mk [Vec.Coeff.u, x ; Vec.Coeff.u, y],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.);
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.z],
					[Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), x] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.of_float (-1.), y] Cs.Vec.Coeff.z;
					 Cs.mk Cstr.Le [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.u, y] (Cs.Vec.Coeff.of_float 3.);
					 Cs.mk Cstr.Lt [Cs.Vec.Coeff.negU, x ; Cs.Vec.Coeff.negU, y] Cs.Vec.Coeff.z],
					 true*)
			]
		|> List.fold_left (fun res (name, point, cstrs, ecstrs, b) ->
			(name, Min.minimize point cstrs, ecstrs, b) :: res) 
			[]
		in
		T.suite "minimize" [
				T.suite "check_points" (List.map check_points tcs);
				T.suite "check_cstrs" (List.map check_cstrs tcs)
			]
	
	let ts : unit -> T.testT
		= fun () -> T.suite Min.name [minimizeTs ()]
end

module Classic = struct
	module Rat = Test(Min.Classic(Vector.Rat.Positive))
	module Float = Test(Min.Classic(Vector.Float.Positive))
	module Symbolic = Test(Min.Classic(Vector.Symbolic.Positive))
	
	let ts : T.testT
		= T.suite "Classic" [Rat.ts (); Float.ts (); Symbolic.ts ()]
end

module Raytracing = struct
	module Glpk = struct
		module Rat = Test(Min.Glpk(Vector.Rat.Positive))
		module Float = Test(Min.Glpk(Vector.Float.Positive))
		module Symbolic = Test(Min.Glpk(Vector.Symbolic.Positive))
		let ts : unit -> T.testT
			= fun () -> T.suite "Glpk" [Rat.ts (); Float.ts (); Symbolic.ts ()]
	end
	module Splx = struct
		module Rat = Test(Min.Splx(Vector.Rat.Positive))
		module Float = Test(Min.Splx(Vector.Float.Positive))
		module Symbolic = Test(Min.Splx(Vector.Symbolic.Positive))
		let ts : T.testT
			= T.suite "Splx" [Rat.ts ();  Float.ts (); Symbolic.ts ()]
	end
	
	let ts : T.testT
		= T.suite "Raytracing" ((if Wrapper.with_glpk then [Glpk.ts ()] else []) @ [Splx.ts])
end

let ts: T.testT
= T.suite "Min" [Classic.ts ; Raytracing.ts]
