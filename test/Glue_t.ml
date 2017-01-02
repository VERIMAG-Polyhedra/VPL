open QArith_base

let x = Pol.Var.fromInt 1
let y = Pol.Var.fromInt 2
let z = Pol.Var.fromInt 3
let a = Pol.Var.fromInt 4

let varPr: Pol.Var.t -> string
= fun _x ->
	let vars: (Pol.Var.t * string) list
	= [x, "x"; y, "y"; z, "z"; a, "a"]
	in
	List.assoc _x vars

let mk l = List.map (fun (i, a) -> (i, Scalar.Rat.mk1 a)) l

let mkc t v c =
  Pol.Cs.mk t (List.map (fun (c, v) -> (Scalar.Rat.mk1 c, v)) v) (Scalar.Rat.mk1 c)

let eq = mkc Cstr.Eq
let le = mkc Cstr.Le
let lt = mkc Cstr.Lt

let joinEltPr: Pol.Cert.joinEltT -> string
= fun j -> Printf.sprintf "{id = %i;\narg1:\n%s\narg2:\n%s\n}"
	(Pol.Cert.getId j) (Pol.Cert.prC (Pol.Cert.getArg1 j)) (Pol.Cert.prC (Pol.Cert.getArg2 j))

let joinPr: Pol.Cert.joinT -> string
= fun j -> String.concat "\n" (List.map joinEltPr j)

module Test ( F : sig val set : unit -> unit end) = struct

	include Pol_t.Test(F)

	(* Glue.inclC *)
	let inclCTs: T.testT
	=
		let chk (name, res, p1, p2) = fun state ->
			if Glue.inclC p1 p2 = res
			then T.succeed state
			else
				match Pol.incl p1 p2 with
				| Pol.NoIncl -> T.fail name "bad result" state
				| Pol.Incl c ->
					let e = Printf.sprintf
			"trying to prove inclusion of p1 in p2 with c\np1:\n%s\np2:\n%s\nc:\n%s"
						(Pol.to_string_ext varPr p1) (Pol.to_string_ext varPr p2) (Pol.Cert.pr c)
					in
					T.fail name e state
		in
		T.suite "incl" (List.map chk inclTcs)

	(* Glue.addC *)
	let addCTs: T.testT
	=
		let chk (name, isEmpty, c, p, _) = fun state ->
			 try 
			   match Glue.addC p c with
			| None ->
				if isEmpty then
					T.succeed state
				else
					T.fail name "unexpectedly empty" state
			| Some p1 ->
				if isEmpty then
					T.fail name "should be empty" state
				else
					if Glue.polChkSync (Some p1) then
						T.succeed state
					else
							T.fail name (CoqPr.charListTr (PedraQ.BasicD.pr (Some p1))) state
		          with
			  | Failure _ ->
				let ce =
					match Pol.add p c with
					| Pol.Added (_, ce) -> ce
					| Pol.Contrad ce -> ce
				in
				let estr =
					Printf.sprintf "error in the checker.\ncertificate:\n%s\n"
						(Pol.Cert.pr ce)
				in
				T.fail name estr state

		in
		T.suite "addC" (List.map chk addTcs)

	(* Glue.joinC *)
	let joinCTs: T.testT
	=
		let chk (name, p1, p2, _) = fun state ->
		  try
			match Glue.joinC p1 p2 with
			| None             -> T.fail name "Bottom" state
			| Some p           ->
				if Glue.polChkSync (Some p) then
					T.succeed state
				else
					T.fail name (CoqPr.charListTr (PedraQ.BasicD.pr (Some p))) state
		  with
			 | Failure _ -> 
				let (p, ce) = Pol.join p1 p2 in
				let e = Printf.sprintf
					"Glue.joinC returned None\ncert:\n%s\np1:\n%s\np2:\n%s\nres:\n%s\n"
					(joinPr ce) (Pol.to_string_ext varPr p1) (Pol.to_string_ext varPr p2)
					(Pol.to_string_ext varPr p)
				in
				T.fail name e state

		in
		T.suite "join" (List.map chk joinTcs)
(*
	(* Glue.getItvC *)
	let getItvCTs
	=
		let chk (name, v, itv, p) state =
			let itv1 = Glue.getItvC p v in
			if eqItv itv itv1 then
				T.succeed state
			else
				T.fail name (Pol.prItv varPr v itv1) state
		in
		T.suite "getItvC" (List.map chk itvizeTcs)
*)
	(* Glue.project *)
	let projectTs: T.testT
	=	(* Checking synchronization between the two components of the result is enough.
		We know the Ocaml result is correct, since we are usings the same test cases
		as in projectTs. *)
		let chk (nm, x, p, r): T.stateT -> T.stateT
		= fun st ->
			 try
			   let p'=Glue.project p x in
				if Glue.polChkSync p'
				then T.succeed st
				else
					let e = "out of sync" in
					T.fail nm e st
			 with
			| Failure _ -> T.fail nm "got None" st
		in
		T.suite "project" (List.map chk projectTcs)

	(* Glue.projectM *)
	let projectMTs: T.testT
	=
		let chk (nm, l, p, _)
		= fun st ->
			try
				let _ = Glue.projectM p l in
				T.succeed st
			with Failure "checking error" -> T.fail nm "got Failure" st
		in
		T.suite "projectM" (List.map chk projectMTcs)

	let ts: T.testT
	= T.suite "Glue" [
		inclCTs;
		addCTs;
		joinCTs;
		(*getItvCTs;*)
		projectTs;
		projectMTs
	]
end

module Classic = Test
	(struct 
		let set : unit -> unit 
			= fun() -> 
			Flags.min := Flags.Classic;
			Flags.proj := Flags.FM;
			Flags.join := Flags.Baryc;
	end)

module PLP_Rat = Test
	(struct 
		let set : unit -> unit 
			= fun() -> 
			Flags.min := Flags.Classic;
			Flags.proj := Flags.PLP Flags.Rat;
			Flags.join := Flags.Join_PLP Flags.Rat;
	end)

module PLP_Float = Test
	(struct 
		let set : unit -> unit 
			= fun() -> 
			Flags.min := Flags.Classic;
			Flags.proj := Flags.PLP Flags.Float;
			Flags.join := Flags.Join_PLP Flags.Float;
	end)

module PLP_Sym = Test
	(struct 
		let set : unit -> unit 
			= fun() -> 
			Flags.min := Flags.Classic;
			Flags.proj := Flags.PLP Flags.Symbolic;
			Flags.join := Flags.Join_PLP Flags.Symbolic;
	end)

let ts : T.testT 
	= T.suite "Pol" [
		Classic.ts;
		PLP_Rat.ts;
		PLP_Float.ts;
		PLP_Sym.ts;
	]
