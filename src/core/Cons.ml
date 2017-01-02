module type Type = sig
	module Cs : Cstr.Rat.Type
	module Cert : Cert.Type with module Cs = Cs

	type 'c t = Cs.t * 'c
	
	val triv : 'c Cert.t -> 'c t 
	val mkTriv : 'c Cert.t -> Cstr.cmpT -> Scalar.Rat.t -> 'c t
	
	val get_c : 'c t -> Cs.t
	val get_cert : 'c t -> 'c
	val to_string : (Cs.Vec.V.t -> string) -> 'c t -> string
	val to_string_ext : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string
	
	val equal : 'c t -> 'c t -> bool 
	
	(** [implies c1 c2] returns [true] if [c1] implies [c2]. *)
	val implies: 'c t -> 'c t -> bool
	val elimc : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c t -> 'c t
	val elim : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> Cs.t -> Cs.t * 'c t
	val rename : 'c Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t -> 'c t
	
	(** [change_variable x lin c cons] proceeds to the change of variable [x = lin + c] in [cons]. *)
	val change_variable : 'c Cert.t -> Cs.Vec.V.t -> Cs.Vec.t -> Cs.Vec.Coeff.t -> 'c t -> 'c t 
	val linear_combination_cert : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c
	val linear_combination_cons : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c t
	
	val add : 'c Cert.t -> 'c t -> 'c t -> 'c t
	val mul : 'c Cert.t -> Scalar.Rat.t -> 'c t -> 'c t
	val split : 'c Cert.t -> 'c t -> 'c t * 'c t
	
	val normalize : 'c Cert.t -> 'c t -> 'c t
end

module Cons (Cs : Cstr.Rat.Type) = struct
	module Cs = Cs
	module Cert = Cert.Cert(Cs)
	type 'c t = Cs.t * 'c
	
	let triv : 'c Cert.t -> 'c t 
		= fun factory ->
		(Cs.eq [] Scalar.Rat.z, factory.Cert.top)
	
	let mkTriv : 'c Cert.t -> Cstr.cmpT -> Scalar.Rat.t -> 'c t
		= fun factory cmp r ->
		(Cs.mk cmp [] r, factory.Cert.triv cmp r)
		
	let get_c (c,_) = c
	
	let get_cert (_,cert) = cert
	
	let to_string : (Cs.Vec.V.t -> string) -> 'c t -> string
		= fun varPr (c,_) -> 
		Cs.to_string varPr c
		
	let to_string_ext : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string
		= fun factory varPr (c,cert) -> 
		Printf.sprintf "%s: %s" (Cs.to_string varPr c) (factory.Cert.to_string cert)
	
	let equal (c1,_) (c2,_) = Cs.equal c1 c2

	let implies (c1,_) (c2,_) = Cs.incl c1 c2
			
	let elim : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> Cs.t -> Cs.t * 'c t
		= fun factory x (eq,eq_cert) cstr ->
		if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v cstr) x) = 0 
		then (cstr, (Cs.eq [] Scalar.Rat.z, factory.Cert.top))
		else
			let (c, n1, n2) = Cs.elim eq cstr x in
			let coeff = Scalar.Rat.div n1 n2 
				|> Scalar.Rat.neg in
			let cstr = Cs.mulc coeff eq in
			let cert = factory.Cert.mul coeff eq_cert in
			(c, (cstr,cert))
					
	let rename : 'c Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t -> 'c t 
		= fun factory fromX toY (c,cert) ->
		let c' = Cs.rename fromX toY c in
		let cert' = factory.Cert.rename fromX toY cert in
		(c',cert')
		
	let linear_combination_cons : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c t
		= fun factory conss witness ->
		List.fold_left
			(fun (cstr_res, cert_res) (i,n) ->
				let (cstr,cert) = List.nth conss i in
				Cs.add
					cstr_res
					(Cs.mulc n cstr)
				,
				factory.Cert.add
					cert_res
					(factory.Cert.mul n cert))
			(triv factory)
			witness
	  	
	let linear_combination_cert : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c
		= fun factory conss witness ->
		try
			List.fold_left
				(fun res (i,n) -> 
					let cert = List.nth conss i 
						|> get_cert 
					in
					factory.Cert.add
						res
						(factory.Cert.mul n cert))
				factory.Cert.top
				witness
	  with _ -> Pervasives.invalid_arg "Cons.linear_combination"
	 
	let add : 'c Cert.t -> 'c t -> 'c t -> 'c t
		= fun factory (c1,cert1) (c2,cert2)->
		(Cs.add c1 c2, factory.Cert.add cert1 cert2)
	
	let mul : 'c Cert.t -> Scalar.Rat.t -> 'c t -> 'c t
		= fun factory r (c,cert) ->
		(Cs.mulc r c, factory.Cert.mul r cert)
				
	let split : 'c Cert.t -> 'c t -> 'c t * 'c t
		= fun factory eq ->
		match get_c eq |> Cs.get_typ with
		| Cstr.Le | Cstr.Lt -> Pervasives.invalid_arg "Cons.split"
		| Cstr.Eq -> 
			let triv = mkTriv factory Cstr.Le Scalar.Rat.z in
			let up = add factory eq triv in
			let low = add factory (mul factory Scalar.Rat.negU eq) triv in
			(up,low)
	
	let normalize : 'c Cert.t -> 'c t -> 'c t
		= fun factory (cstr, cert) ->
		let gcd = Cs.Vec.gcd (Cs.get_v cstr) in
		mul factory gcd (cstr, cert)
		
	let elimc : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c t -> 'c t
		= fun factory x (c1,cert1) (c2,cert2) ->
			let (c, n1, n2) = Cs.elim c1 c2 x in
			(c, Cert.linear_combination factory [cert1, n1; cert2, n2])
			|> normalize factory
			
	type ('c1,'c2) discr_t = 'c1 * 'c2
	
	type ('c1,'c2) discr_cert = (('c1,'c2) discr_t) Cert.t
	
	let discr_factory : 'c1 Cert.t -> 'c2 Cert.t -> ('c1,'c2) discr_cert
		= fun fac1 fac2 ->
		Cert.({ 
			name = "discr"; 
			top = (fac1.top, fac2.top);
			triv = (fun cmp r -> fac1.triv cmp r, fac2.triv cmp r);  
			add  = (fun (c1,c2) (c1',c2') -> fac1.add c1 c1', fac2.add c2 c2');    
			mul = (fun r (c,c') -> fac1.mul r c, fac2.mul r c');
			merge = (fun (c1,c2) (c1',c2') -> fac1.merge c1 c1', fac2.merge c2 c2'); 
			to_le = (fun (c,c') -> fac1.to_le c, fac2.to_le c');
			to_string = (fun (c,c') -> (fac1.to_string c) ^ "( ; )"  ^ (fac2.to_string c'));
			rename = (fun fromX toY (c,c') -> fac1.rename fromX toY c, fac2.rename fromX toY c');
		})
	
	let joinSetup_1 : 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c1 t 
		-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (('c1,'c2) discr_t) t
		= fun factory2 nxt relocTbl alpha (c,cert) ->
		let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
		let (vec2, alphaCoef, cst) = (vec1, Cs.Vec.Coeff.neg (Cs.get_c c), Cs.Vec.Coeff.z)
		in
		let c' = {c with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
		let cert' = (cert, factory2.Cert.top) in
		(nxt1, relocTbl1, (c',cert'))

	let joinSetup_2 : 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c2 t 
		-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (('c1,'c2) discr_t) t
		= fun factory1 nxt relocTbl alpha (c,cert) ->
		let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
		let (vec2, alphaCoef, cst) = (Cs.Vec.add (Cs.get_v c) (Cs.Vec.neg vec1), Cs.get_c c, Cs.get_c c)
		in
		let c' = {c with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
		let cert' = (factory1.Cert.top, cert) in
		(nxt1, relocTbl1, (c',cert'))
	(*
	let change_variable : 'c Cert.t -> Cs.Vec.V.t -> Cs.Vec.t -> Cs.Vec.Coeff.t -> 'c t -> 'c t 
		= fun factory x lin c (cstr,cert) ->
		let c' = Cs.change_variable x lin c cstr in
		let cert' = factory.Cert.change_variable x lin c cert in
		(c',cert')
	
	let change_variables : 'c Cert.t -> (Cs.Vec.V.t * Cs.Vec.t * Cs.Vec.Coeff.t) list -> 'c t -> 'c t 
		= fun factory l (cstr,cert) ->
		List.fold_left 
			(fun (cstr,cert) (x,lin,c) -> 
				change_variable factory x lin c (cstr,cert))
			(cstr,cert) l
	
	type join_data = {
		alpha : Cs.Vec.V.t;
		alpha' : Cs.Vec.V.t;
		alpha'' : Cs.Vec.V.t;
		y' : (Cs.Vec.V.t -> Cs.Vec.V.t);
		}
			
	let toy' : 'c Cert.t -> join_data -> 'c t -> 'c t
		= fun factory data (cstr,cert) ->
		let vars = Cs.getVars [cstr] |> Cs.Vec.V.Set.elements in
		let l = List.fold_left
			(fun l v -> 
				let lin = Cs.Vec.mk [Cs.Vec.Coeff.u, data.y' v] in
				(v, lin, Cs.Vec.Coeff.z) :: l)
			[] vars
		in
		change_variables factory l (cstr,cert)
	
	(* y'' = x - y' *)
	let y''_to_x_minus_y' : 'c Cert.t -> join_data -> 'c t -> 'c t
		= fun factory data (cstr,cert) ->
		let vars = Cs.getVars [cstr] |> Cs.Vec.V.Set.elements in
		let l = List.fold_left
			(fun l x -> 
				let lin = Cs.Vec.mk [Cs.Vec.Coeff.u, x ; Cs.Vec.Coeff.negU, data.y' x] in
				(x, lin, Cs.Vec.Coeff.z) :: l)
			[] vars
		in
		change_variables factory l (cstr,cert)
	
	(* alpha'' = 1 - alpha' *)
	let alpha''_to_1_minus_alpha' : 'c Cert.t -> join_data -> 'c t -> 'c t
		= fun factory data (cstr,cert) ->
		let lin = Cs.Vec.mk [Cs.Vec.Coeff.negU, data.alpha'] in
		let l = [data.alpha'', lin, Cs.Vec.Coeff.u] in
		change_variables factory l (cstr,cert)
	*)
	(*
	let mul_alpha : 'c Cert.t -> join_data -> 'c t -> 'c t
		= fun factory data cons ->
		let cste
		*)
			(*
	let joinSetup: 'c Cert.t -> bool -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c t -> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * 'c t
	= fun factory dup nxt relocTbl alpha c ->
		let cstr = get_c c in
		let cert = get_cert c in
		let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v cstr) relocTbl in
		let (vec2, alphaCoef, cst, cert') =
			if dup then
				let cert' = factory.change_variable cert 
				(Cs.Vec.add (Cs.get_v cstr) (Cs.Vec.neg vec1), 
				 Cs.get_c cstr, 
				 Cs.get_c cstr,
				 cert')
			else
				(vec1, Cs.Vec.Coeff.neg (Cs.get_c cstr), Cs.Vec.Coeff.z)
		in
		let c1 = {cstr with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
		(nxt1, relocTbl1, (c1,cert'))
*)
end	
(*(** The information which is ultimately stored in the inclusion certificates is manipulated
explicitely during the calculations carried on polyhedra.
This module extends the representation of affine constraints to record this information on-the-fly. *)

module type Type = sig
	module Cs : Cstr.Rat.Type
	module Cert : Cert.Type with module Cs = Cs
	type t = { id: int; f: Cert.frag_t; c: Cs.t }

	val get_id : t -> int
	val get_f : t -> Cert.frag_t
	val get_c : t -> Cs.t

	val mk: int -> Cs.t -> t

	val implies: t -> t -> bool
	val equal: t -> t -> bool
	val rename: Cs.Vec.V.t -> Cs.Vec.V.t -> t -> t

	val joinSetup: bool -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> int -> t -> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * t

	val initc: t -> t
	val extractc: t -> int * Cert.cstr_t

	val elimc: int -> Cs.Vec.V.t -> t -> t -> t

	val to_string: (Cs.Vec.V.t -> string) -> t -> string
	val to_string_ext: (Cs.Vec.V.t -> string) -> t -> string
end

module Cons (Cs : Cstr.Rat.Type) = struct
	module Cs = Cs
	module Cert = Cert.Cert(Cs)
	type t = { id: int; f: Cert.frag_t; c: Cs.t }

	let get_id (x : t) = x.id
	let get_f (x : t) = x.f
	let get_c (x : t) = x.c

	let mk id c = { id = id; f = Cert.mkF [id, Cs.Vec.Coeff.u]; c = c }

	let implies c1 c2 = Cs.incl c1.c c2.c
	let equal c1 c2 = Cs.equal c1.c c2.c

	let rename fromX toY c =
		let v = Cs.get_v c.c in
		let v1 = Cs.Vec.set v fromX Cs.Vec.Coeff.z in
		let v2 = Cs.Vec.set v1 toY (Cs.Vec.get v fromX) in
		assert (Cs.Vec.Coeff.cmpz (Cs.Vec.get v toY) = 0);
		{c with c = {c.c with Cs.v = v2}}

	let joinSetup: bool -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> int -> t -> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * t
	= fun dup nxt relocTbl alpha idOffset c ->
		let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c.c) relocTbl in
		let (vec2, alphaCoef, cst) =
			if dup then
				(Cs.Vec.add (Cs.get_v c.c) (Cs.Vec.neg vec1), Cs.get_c c.c, Cs.get_c c.c)
			else
				(vec1, Cs.Vec.Coeff.neg (Cs.get_c c.c), Cs.Vec.Coeff.z)
		in
		let c1 = {c.c with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
		(nxt1, relocTbl1, mk (idOffset + c.id) c1)

	let initc c = { c with f = Cert.mkF [c.id, Cs.Vec.Coeff.u] }
	let extractc c = (c.id, Cert.Direct c.f)

	let elimc id x c1 c2 =
		let (c, n1, n2) = Cs.elim c1.c c2.c x in
		{ id = id; f = Cert.mergeF [c1.f, n1; c2.f, n2]; c = c }

	let to_string_ext: (Cs.Vec.V.t -> string) -> t -> string
	= fun varPr c -> Printf.sprintf "(%i): %s (frag: %s)" c.id (Cs.to_string varPr c.c) (Cert.prF c.f)

	let to_string: (Cs.Vec.V.t -> string) -> t -> string
	= fun varPr c -> Cs.to_string varPr c.c
end
*)
