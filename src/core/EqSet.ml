module type Type = sig
	
	module Cs : Cstr.Rat.Type
	module Cons : Cons.Type with module Cs = Cs
	module Cert = Cons.Cert
	
	type 'c t = (Cs.Vec.V.t * 'c Cons.t) list

	val to_string: (Cs.Vec.V.t -> string) -> 'c t -> string
	val to_string_ext: 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string

	type 'c rel_t =
		| NoIncl
		| Incl of 'c list
	
	val nil : 'c t
	
	val isTop: 'c t -> bool
	val list : 'c t -> 'c Cons.t list
	
	(** [filter factory s c] replaces in [c] each variable defined in [s] by its definition. *)
	val filter : 'c Cert.t -> 'c t -> 'c Cons.t -> 'c Cons.t
	
	(** [filter2 factory eqset cstr] returns a couple [(cstr',cert)] where 
	{ul 
		{- [cstr'] is [cstr] where variables have been substitued by their definition in [eqset].}
		{- [cert] is the combination of constraints of [eqset] that must be added to [cstr] to obtain [cstr']}
	}.
	For instance, [filter2 f (x = 2y+1) (2x <= 1)] returns [(4y<=-1, 2x - 4y = 2)].*)
	val filter2 : 'c Cert.t -> 'c t -> Cs.t -> Cs.t * 'c
	
	val implies : 'c Cert.t -> 'c t -> 'c Cons.t -> bool
	
	val incl : 'c1 Cert.t -> 'c1 t -> 'c2 t -> 'c1 rel_t
	
	(** Does not check certificates. *)
	val equal: 'c1 t -> 'c2 t -> bool
	
	val choose : Cs.t -> Cs.Vec.V.t * Cs.Vec.Coeff.t 

	val rename: 'c Cert.t -> 'c t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t
	
	val pick: Cs.Vec.V.t option Rtree.t -> 'c Cons.t -> Cs.Vec.V.t option
	
	(** [subst factory x c s] substitutes [x] in [s] by its definition in [c]. *)
	val subst: 'c Cert.t -> Cs.Vec.V.t -> 'c Cons.t -> 'c t -> 'c t

	val tryDefs: 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t

	val trySubstM: 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t

	val trySubst: 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c Cons.t option * 'c t 
	
	type 'c meetT =	
	| Added of 'c t
	| Bot of 'c
	
	val meetEq: 'c meetT -> 'c meetT -> bool
	val meet_to_string : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c meetT -> string
	
	val addM: 'c Cert.t -> 'c t -> 'c Cons.t list -> 'c meetT
	val add: 'c Cert.t -> 'c t -> 'c Cons.t -> 'c meetT
end

module EqSet(Cs : Cstr.Rat.Type) = struct
	
	module Cs = Cs
	module Cons = Cons.Cons(Cs)
	module Cert = Cons.Cert
	
	type 'c t = (Cs.Vec.V.t * 'c Cons.t) list

	let to_string: (Cs.Vec.V.t -> string) -> 'c t -> string
		= fun varPr e ->
			List.fold_right 
			(fun (_, c) s -> s ^ (Cons.to_string varPr c) ^ "\n") e ""

	let to_string_ext: 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string
		= fun factory varPr e->
			List.fold_right 
			(fun (x, c) s -> Printf.sprintf "%s(%s, %s)\n"
				s (varPr x) (Cons.to_string_ext factory varPr c)) e ""
			
	type 'c rel_t =
		| NoIncl
		| Incl of 'c list
	
	let nil : 'c t = []
	
	let isTop (s : 'c t) = (s = [])
	
	let list x = List.map Pervasives.snd x
	
	let equal (s1 : 'c1 t) (s2 : 'c2 t) =
		if List.length s1 = List.length s2 then
			List.for_all2 
				(fun (x1 ,(c1,_)) (x2, (c2,_)) ->
					x1 = x2 && Cs.inclSyn c1 c2) 
				s1 s2
		else
			false
	
	(* L'ordre fold_right est important pour les réécritures *)
	let filter : 'c Cert.t -> 'c t -> 'c Cons.t -> 'c Cons.t
		= fun factory s c ->
		let filter1 (x, (c2,cert2)) (c1,cert1) =
			if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v c1) x) = 0 then
				(c1,cert1)
			else
				Cons.elimc factory x (c1,cert1) (c2,cert2)
		in
		List.fold_right filter1 s c
	
	let filter2 : 'c Cert.t -> 'c t -> Cs.t -> Cs.t * 'c Cons.t
		= fun factory s c ->
		let filter1 (c_res,(cons_c, cons_cert)) (x, (c,cert)) =
			let (c_res',(cstr_res',cert_res')) = Cons.elim factory x (c,cert) c_res in
			(c_res', (Cs.add cstr_res' cons_c, factory.Cert.add cert_res' cons_cert))
		in
		List.fold_left filter1 (c, Cons.triv factory) s
		
	let implies : 'c Cert.t -> 'c t -> 'c Cons.t -> bool
		= fun factory s (c,cert) ->
		let (c',cert') = filter factory s (c,cert) in
		match Cs.tellProp c' with
		| Cs.Trivial -> true
		| Cs.Contrad (* should tell something useful *)
		| Cs.Nothing -> false
		
	
	let incl : 'c1 Cert.t -> 'c1 t -> 'c2 t -> 'c1 rel_t
		= fun factory s1 s2 ->
		if List.length s1 < List.length s2 then
			NoIncl
		else
			let rec _incl certs =
			function
			| [] -> Incl certs
			| (_, (c,cert))::t ->
				let (c2,(_,cert2)) = filter2 factory s1 c in
				match Cs.tellProp c2 with
				| Cs.Contrad | Cs.Nothing -> NoIncl
				| Cs.Trivial ->
					_incl (cert2::certs) t
			in
			_incl [] s2	
	

	let choose : Cs.t -> Cs.Vec.V.t * Cs.Vec.Coeff.t 
		= fun c ->
		match Cs.Vec.M.findPred (fun n -> Cs.Vec.Coeff.cmpz n <> 0) (Cs.get_v c) with
		| None -> failwith "EqSet.choose"
		| Some (x, a) -> (x, a)

	let rename factory s fromX toY =
		let rename1 (x, c) =
			((if x = fromX then toY else x), Cons.rename factory fromX toY c)
		in
		List.map rename1 s
	
	let pick (msk : Cs.Vec.V.t option Cs.Vec.M.t) ((e,cert) : 'c Cons.t) =
		match Cs.Vec.M.findPred2 
			(fun n1 n2 -> n1 <> None && not (Cs.Vec.Coeff.cmpz n2 = 0)) 
			msk (Cs.get_v e)
		with
		| Some (_,Some n1,n2) -> Some n1
		| _ -> None 
	
	let rec subst : 'c Cert.t -> Cs.Vec.V.t -> 'c Cons.t -> 'c t -> 'c t
		= fun factory x e ->
		function
		| [] -> []
		| (x1, (e1,cert1))::l1 ->
			if Cs.Vec.V.equal x1 x then
				failwith "EqSet.subst"
			else
				let e2 =
					if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v e1) x) = 0 then
						(e1,cert1)
					else
						Cons.elimc factory x e (e1,cert1)
				in
				(x1,e2) :: subst factory x e l1
				
	let rec tryDefs : 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t
		= fun factory msk ->
		function
		| [] -> (None, [])
		| (x, e)::l ->
			if Cs.Vec.M.get None msk x = None 
			then
				let (def, l1) = tryDefs factory msk l in
				(def, (x, e)::l1)
			else
				let l1 = subst factory x e l in
				(Some (e, x), l1)
	
	let trySubstM : 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t
		= fun factory msk l ->
		let (def, l1) = tryDefs factory msk l in
		if def = None then
			let rec _trysubstm msk =
				function
				| [] -> (None, [])
				| (x, e)::l ->
					match pick msk e with
					| None ->
						let (def, l1) = _trysubstm msk l in
						(def, (x, e)::l1)
					| Some x ->
						let l1 = subst factory x e l in
						(Some (e, x), l1)
			in
			_trysubstm msk l
		else
			(def, l1)
			
	let trySubst : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c Cons.t option * 'c t 
		= fun factory x l ->
		let msk = Cs.Vec.M.set None Cs.Vec.M.empty x (Some x) in
		let (optx, s1) = trySubstM factory msk l in
		match optx with
		| None -> (None, s1)
		| Some (e, _) -> (Some e, s1)

	type 'c meetT =	
	| Added of 'c t
	| Bot of 'c
	
	(* XXX: doit on comparer les certificats? *)
	let meetEq: 'c meetT -> 'c meetT -> bool
		= fun ar ar' ->
		match ar, ar' with
		| Added e, Added e' -> equal e e'
		| Bot f, Bot f' -> true
		| Added _, Bot _
		| Bot _, Added _ -> false
	
	let meet_to_string : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c meetT -> string
	= fun factory varPr -> function
		| Added e ->
			Printf.sprintf "Added %s" (to_string_ext factory varPr e)
		| Bot f -> Printf.sprintf "Bot : %s" (factory.Cert.to_string f)
	
	let addM: 'c Cert.t -> 'c t -> 'c Cons.t list -> 'c meetT
		= fun factory s conss ->
		let add : 'c Cons.t -> 'c meetT -> 'c meetT
			= fun (c,cert) ->
			function
			| Bot _ as r -> r
			| Added s ->
				match Cs.get_typ c with
				| Cstr.Le | Cstr.Lt -> failwith "EqSet.addM"
				| Cstr.Eq ->
					let (c1,cert1) = filter factory s (c,cert) in
				 	match Cs.tellProp c1 with
					| Cs.Trivial -> Added s
					| Cs.Contrad -> Bot cert1
					| Cs.Nothing ->
						let (x, ax) = choose c1 in
						let c2 = Cs.mulc (Cs.Vec.Coeff.inv ax) c1
						and cert2 = factory.Cert.mul (Cs.Vec.Coeff.inv ax) cert1 in
						(* rewritting of the rest of the equality set with the new one. *)
						let s' = if !Flags.row_echelon_equalities
							then subst factory x (c2,cert2) s 
							else s
						in
						Added ((x, (c2,cert2)) :: s')
		in
		List.fold_left (fun res c -> add c res) (Added s) conss
		
		
	let add: 'c Cert.t -> 'c t -> 'c Cons.t -> 'c meetT
		= fun factory s c -> 
		addM factory s [c]
	
	let joinSetup_1: 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c1 t 
		-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list
		= fun factory2 nxt relocTbl alpha s ->
		let apply (x, c) (nxt1, relocTbl1, s1) =
			let (nxt2, relocTbl2, c1) = Cons.joinSetup_1 factory2 nxt1 relocTbl1 alpha c in
			let x1 =	match Cs.Vec.M.get None relocTbl2 x with
					| None -> failwith "EqSet.joinSetup_1"
					| Some x1 -> x1
			in
			(nxt2, relocTbl2, (x1, c1)::s1)
		in
	(* List.fold_right is necessary because order needs to be preserved (echelon form) *)
		List.fold_right apply s (nxt, relocTbl, nil)
	
	let joinSetup_2: 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c2 t 
		-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list
		= fun factory1 nxt relocTbl alpha s ->
		let apply (x, c) (nxt1, relocTbl1, s1) =
			let (nxt2, relocTbl2, c1) = Cons.joinSetup_2 factory1 nxt1 relocTbl1 alpha c in
			let x1 = x in
			(nxt2, relocTbl2, (x1, c1)::s1)
		in
	(* List.fold_right is necessary because order needs to be preserved (echelon form) *)
		List.fold_right apply s (nxt, relocTbl, nil)
		
end
	(*(** {!module: Pol} treats equalities and inequalities differently.
This module is the data structure that hosts the equalities of a polyhedron. *)

module type Type = sig
	module Cs : Cstr.Rat.Type
	(*module Cons : Cons.Type with module Cs = Cs*)
	type 'c t = (Cs.Vec.V.t * 'c Cons.t) list

	type 'c rel_t =
	| NoIncl
	| Incl of 'c Cons.Cert.t list

	val nil: 'c t

	val filter: 'c t -> 'c Cons.t -> 'c Cons.t

	val choose: 'c Cons.t -> Cs.Vec.V.t * Cs.Vec.Coeff.t

	val implies: 'c t -> 'c Cons.t -> bool
	val incl: 'c t -> 'c t -> 'c rel_t
	val equal: 'c t -> 'c t -> bool

	val rename: 'c t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t

	val pick: Cs.Vec.V.t option Rtree.t -> 'c Cons.t -> Cs.Vec.V.t option
	val subst: Cs.Vec.V.t -> int -> Cons.t -> t -> int * t

	val tryDefs: int -> Cs.Vec.V.t option Rtree.t -> t ->
		(Cons.t * Cs.Vec.V.t) option * int * t

	val trySubstM: int -> Cs.Vec.V.t option Rtree.t -> t ->
		(Cons.t * Cs.Vec.V.t) option * int * t

	val trySubst: Cs.Vec.V.t -> int -> t -> Cons.t option * int * t

	val joinSetup: bool -> Cs.Vec.V.t -> Cs.Vec.V.t option Rtree.t -> Cs.Vec.V.t -> int -> t -> Cs.Vec.V.t * Cs.Vec.V.t option Rtree.t * t

	val extractc: t -> (int * Cons.Cert.cstr_t) list
	val initc: t -> t

	val list: t -> Cons.t list
	val isTop: t -> bool

	val to_string : (Cs.Vec.V.t -> string) -> t -> string
	val to_string_ext: (Cs.Vec.V.t -> string) -> t -> string
	
	(** The result of an intersection.
	Intersection is just concatenation and then minimization.
	Either the result is an unsatisfiable equality set,
	which can be shown by a linear combination of equalities
	yielding a trivially contradictory equality.
	This is the semantics of [Bot f].
	Or the result is an equality set which is equivalent to
	the union of a pre-existing set and new equalities.
	This is the semantics of [Added (s, cert)]. *)
	type meetT = 
		| Added of t * Cons.Cert.partialT
		| Bot of Cons.Cert.frag_t

	(** Pretty-print a value of type [meetT]. *)
	val meetPr: (Cs.Vec.V.t -> string) -> meetT -> string

	(** Test whether two values of type [meetT] are equal.
	Equality on [Cons.Cert.frag_t] is tested using [Cons.Cert.equalF] and
	equality on t is tested using [equal] *)
	val meetEq: meetT -> meetT -> bool

	(** [addM s l] computes the intersection of [s] with the constraints [l].
	The integer provided with each constraint will identify it uniquely in the
	set. It is considered different from all identifiers in [s], although this
	is not checked. The linear combination which are returned as part of the
	result value are expressed with respected to the input constraints. *)
	val addM: t -> (int * Cs.t) list -> meetT

	(** [add s i c] is a short-hand notation for [addM s [i, c]]. *)
	val add: t -> int -> Cs.t -> meetT

	(** [meet s s'] computes the intersection of [s] with [s'].
	Constraint identifiers in both [s] and [s'] must be unique in the union
	of the two sets. [meet] is implemented in terms of [addM]. *)
	val meet: t -> t -> meetT
end

module EqSet(Cs : Cstr.Rat.Type) = struct
	module Cs = Cs
	module Cons = Cons.Cons(Cs)
	
	type t = (Cs.Vec.V.t * Cons.t) list

	let to_string: (Cs.Vec.V.t -> string) -> t -> string
	= fun varPr e ->
		List.fold_right (fun (_, c) s -> s ^ (Cons.to_string varPr c) ^ "\n") e ""

	let to_string_ext: (Cs.Vec.V.t -> string) -> t -> string
	= fun varPr e ->
		let pr1 x c = Printf.sprintf "%s (def: %s)\n" (Cons.to_string_ext varPr c) (varPr x) in
		List.fold_right (fun (x, c) s -> s ^ (pr1 x c)) e ""

	type rel_t =
	| NoIncl
	| Incl of (int * Cons.Cert.cstr_t) list

	let nil : t = []

	let initc (s : t) = List.map (fun (x, c) -> (x, Cons.initc c)) s
	let extractc (s : t) = List.map (fun (_, c) -> Cons.extractc c) s

	let filter s c =
		let filter1 (x, c2) c1 =
			if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v (Cons.get_c c1)) x) = 0 then
				c1
			else
				Cons.elimc (Cons.get_id c1) x c1 c2
	 	in
		List.fold_right filter1 s c

	let implies s c =
		match Cs.tellProp (Cons.get_c (filter s c)) with
		| Cs.Trivial -> true
		| Cs.Contrad (* should tell something useful *)
		| Cs.Nothing -> false

	let incl (s1 : t) (s2 : t) =
		if List.length s1 < List.length s2 then
			NoIncl
		else

		let s = initc s1 in
		let rec _incl cert =
		function
		| [] -> Incl cert
		| (_, c)::t ->
			(* XXX: Could this be done without the -1 dance? *)
			let c1 = filter s (Cons.mk (-1) (Cons.get_c c)) in
			match Cs.tellProp (Cons.get_c c1) with
			| Cs.Contrad | Cs.Nothing -> NoIncl
			| Cs.Trivial ->
				let (d, w) = List.partition (fun (id, _) -> id = -1) (Cons.get_f c1) in
				match d with
				| [_, a] ->
					let norm (i, a1) = (i, Cs.Vec.Coeff.div a1 (Cs.Vec.Coeff.neg a)) in
					let f = Cons.Cert.mkF (List.map norm w) in
					_incl (((Cons.get_id c), Cons.Cert.Direct f)::cert) t

				| _ -> failwith "EqSet.incl"
		in
		_incl [] s2

	let equal (s1 : t) (s2 : t) =
		if List.length s1 = List.length s2 then
			List.for_all2 (fun (x1 ,c1) (x2, c2) ->
				x1 = x2 && Cs.inclSyn (Cons.get_c c1) (Cons.get_c c2)) s1 s2
		else
			false
			
	let choose c =
		match Cs.Vec.M.findPred (fun n -> Cs.Vec.Coeff.cmpz n <> 0) (Cs.get_v (Cons.get_c c)) with
		| None -> failwith "EqSet.choose"
		| Some (x, a) -> (x, a)

	type addInternalT
	=	| AddCont of t * Cons.Cert.partialT
		| AddBot of Cons.Cert.frag_t

	let rename s fromX toY =
		let rename1 (x, c) =
			((if x = fromX then toY else x), Cons.rename fromX toY c)
		in
		List.map rename1 s

	let pick (msk : Cs.Vec.V.t option Cs.Vec.M.t) (e : Cons.t) =
		match Cs.Vec.M.findPred2 
			(fun n1 n2 -> n1 <> None && not (Cs.Vec.Coeff.cmpz n2 = 0)) 
			msk (Cs.get_v (Cons.get_c e))
		with
		| Some (_,Some n1,n2) -> Some n1
		| _ -> None 

	let rec subst x i e =
		function
		| [] -> (i, [])
		| (x1, e1)::l1 ->
			if x1 = x then
				failwith "EqSet.subst"
			else
				let e2 =
					if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v (Cons.get_c e1)) x) = 0 then
						{ e1 with Cons.id = i }
					else
						Cons.elimc i x e e1
				in
				let (i1, l2) = subst x (i + 1) e l1 in
				(i1, (x1, e2)::l2)

	let rec tryDefs (i : int) (msk : Cs.Vec.V.t option Cs.Vec.M.t) =
		function
		| [] -> (None, i, [])
		| (x, e)::l ->
			if Cs.Vec.M.get None msk x = None then
				let (def, i1, l1) = tryDefs (i + 1) msk l in
				(def, i1, (x, { e with Cons.id = i })::l1)
			else
				let (i1, l1) = subst x i e l in
				(Some (e, x), i1, l1)

	let trySubstM i msk l =
		let (def, i1, l1) = tryDefs i msk l in
		if def = None then
			let rec _trysubstm i msk =
				function
				| [] -> (None, i, [])
				| (x, e)::l ->

				match pick msk e with
				| None ->
					let e1 = { e with Cons.id = i } in
					let (def, i1, l1) = _trysubstm (i + 1) msk l in
					(def, i1, (x, e1)::l1)

				| Some x ->
					let (i1, l1) = subst x i e l in
					(Some (e, x), i1, l1)
			in
			_trysubstm i msk l
		else
			(def, i1, l1)

	let trySubst x i l =
		let msk = Cs.Vec.M.set None Cs.Vec.M.empty x (Some x) in
		let (optx, i1, s1) = trySubstM i msk l in
		match optx with
		| None -> (None, i1, s1)
		| Some (e, _) -> (Some e, i1, s1)

	let joinSetup: bool -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> int -> t -> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * t
	= fun dup nxt relocTbl alpha idOffset s ->
		let apply (x, c) (nxt1, relocTbl1, s1) =
			let (nxt2, relocTbl2, c1) = Cons.joinSetup dup nxt1 relocTbl1 alpha idOffset c in
			let x1 =
				if dup then
					x
				else
					match Cs.Vec.M.get None relocTbl2 x with
					| None -> failwith "EqSet.joinSetup2"
					| Some x1 -> x1
			in
			(nxt2, relocTbl2, (x1, c1)::s1)
		in
	(* List.fold_right is necessary because order needs to be preserved (echelon form) *)
		List.fold_right apply s (nxt, relocTbl, nil)

	let list (s : t) = List.map (fun (_, c) -> c) s
	let isTop (s : t) = (s = [])

	type meetT
	=	| Added of t * Cons.Cert.partialT
		| Bot of Cons.Cert.frag_t

	let meetPr: (Cs.Vec.V.t -> string) -> meetT -> string
	= fun varPr -> function
		| Added (e, ce) ->
			Printf.sprintf "Added (s, ce) with\ns:\n%s\nce:\n%s"
				(to_string_ext varPr e) (Cons.Cert.partialPr ce)
		| Bot f -> Printf.sprintf "Bot: %s" (Cons.Cert.prF f)

	let meetEq: meetT -> meetT -> bool
	= fun ar ar' ->
		match ar, ar' with
		| Added (e, ce), Added (e', ce') ->
			equal e e' && Cons.Cert.partialEq ce ce'
		| Bot f, Bot f' -> Cons.Cert.isEqF f f'
		| Added (_, _), Bot _
		| Bot _, Added (_, _) -> false

	let addM: t -> (int * Cs.t) list -> meetT
	=	let switch: int -> Cons.Cert.frag_t -> Cs.Vec.Coeff.t * Cons.Cert.frag_t
		= fun i f ->
			let (f1, f2) = List.partition (fun (i', _) -> i = i') f in
			match f1 with
			| [_, n] ->
				let invN = Cs.Vec.Coeff.inv n in
				(invN, Cons.Cert.mapF (fun n' -> Cs.Vec.Coeff.mul (Cs.Vec.Coeff.neg invN) n') f2)
			| _ -> failwith "EqSet.addM"
		in
		let add: addInternalT -> int * Cs.t -> addInternalT
		= function
			| AddBot _ as r -> fun _ -> r
			| AddCont (s, red) ->
				fun (i, c) ->
					match Cs.get_typ c with
					| Cstr.Le | Cstr.Lt -> failwith "EqSet.addM"
					| Cstr.Eq ->
						let c1 = filter s (Cons.mk i c) in
						match Cs.tellProp (Cons.get_c c1) with
						| Cs.Trivial ->
							let frag = Pervasives.snd (switch i (Cons.get_f c1)) in
							AddCont (s, (i, Cons.Cert.Direct frag)::red)
						| Cs.Contrad -> AddBot (Cons.get_f c1)
						| Cs.Nothing ->
							let (x, ax) = choose c1 in
							let c2 = Cs.mulc (Cs.Vec.Coeff.inv ax) (Cons.get_c c1) in
							let f2: Cons.Cert.frag_t
							=	let f = fun n -> Cs.Vec.Coeff.mul (Cs.Vec.Coeff.inv ax) n in
								Cons.Cert.mapF f (Cons.get_f c1)
							in
							let newE = {Cons.id = i; Cons.c = c2; Cons.f = f2} in
							AddCont ((x, newE)::s, red)
		in
		fun s0 l ->
			let s0' = initc s0 in
			match List.fold_left add (AddCont (s0', [])) l with
			| AddBot f -> Bot f
			| AddCont (s, red) ->
				let fl = List.map (fun (_, c) -> (Cons.get_id c, Cons.get_f c)) s in
				let fwdCe = List.map (fun (i, f) -> (i, Cons.Cert.Direct f)) fl in
				Added (s, fwdCe)

	let add: t -> int -> Cs.t -> meetT
	= fun s i c -> addM s [i, c]

	let meet: t -> t -> meetT
	= fun s s' -> addM s (List.map (fun (_, c) -> (Cons.get_id c, Cons.get_c c)) s')
	
end
*)
