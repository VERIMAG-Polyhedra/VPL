(** Certificate types and operations on them. *)

module type Type = sig
	module Cs : Cstr.Rat.Type
	
	type 'c t = 
	  { name : string; 
		 top : 'c; (* NB: top = triv Cstr.Eq Nb.z *)
		 triv: Cstr.cmpT -> Scalar.Rat.t -> 'c;  
		 add : 'c -> 'c -> 'c;    
		 mul : Scalar.Rat.t -> 'c -> 'c;  (* in [mul n c], [n] must be non-negative except if [c] is an equality ! *)
		 merge : 'c -> 'c -> 'c;  (* corresponds to ill-named "SplitEq" *)
		 to_le : 'c -> 'c;
		 to_string : 'c -> string;
		 rename : Cs.Vec.V.t -> Cs.Vec.V.t -> 'c -> 'c; (* Rename fromX toY cert *)
	  }

	val linear_combination : 'c t -> ('c * Scalar.Rat.t) list -> 'c
end

module Cert (Cs : Cstr.Rat.Type) = struct
	module Cs = Cs
	
	type 'c t = 
	  { name : string; 
		 top : 'c; (* NB: top = triv Cstr.Eq Nb.z *)
		 triv: Cstr.cmpT -> Scalar.Rat.t -> 'c;  
		 add : 'c -> 'c -> 'c;    
		 mul : Scalar.Rat.t -> 'c -> 'c;  (* in [mul n c], [n] must be non-negative except if [c] is an equality ! *)
		 merge : 'c -> 'c -> 'c;  (* corresponds to ill-named "SplitEq" *)
		 to_le : 'c -> 'c;
		 to_string : 'c -> string;
		 rename : Cs.Vec.V.t -> Cs.Vec.V.t -> 'c -> 'c;
	  }

	let linear_combination : 'c t -> ('c * Scalar.Rat.t) list -> 'c
		= fun factory l ->
			List.fold_left
				(fun res (cert,n) ->
					factory.add
						res
						(factory.mul n cert))
				(factory.top)
				l
end
(*
module type Type = sig
	module Cs : Cstr.Rat.Type

	(** All the certificates aim at proving inclusion properties,
	which can be proved by exhibiting linear combinations of constraints.
	[frag_t] specifies such a linear combination of constraints as a list of pairs.
	Each pair gives the coefficient to be applied to a constraint identified by a number
	(see {! Cons.t}). *)
	type frag_t = (int * Cs.Vec.Coeff.t) list

	(** Pretty-print a value of type [frag_t]. *)
	val prF: frag_t -> string

	(** [isEqF f1 f2] returns [true] if [f1] and [f2] specify the same
	linear combination.  The order of the coefficients is irrelevant. *)
	val isEqF: frag_t -> frag_t -> bool

	(** Build a [frag_t] for a coefficient list.
	[mkF] checks that none of the coefficients is nil
	and that each constraint does appear more than once.
	[Invalid_argument "Cert.mkF"] is raised if one of these conditions is not met.
	Well-formedness is relied on by partial certificate manipulations in {! Pol}. *)
	val mkF: (int * Cs.Vec.Coeff.t) list -> frag_t

	(** Apply a given function to all the coefficients of a [frag_t]. *)
	val mapF: (Cs.Vec.Coeff.t -> Cs.Vec.Coeff.t) -> frag_t -> frag_t

	(** [rename idOld idNew f] replaces the coefficient of the contraint
	identified by [idOld] by equal coefficients of the contraint identified
	by [idNew] in [f]. *)
	val renameF: int -> int -> frag_t -> frag_t

	(** Make linear combinations of fragments. *)
	val mergeF: (frag_t * Cs.Vec.Coeff.t) list -> frag_t

	(** This type expresses the arguments of a proof of inclusion of a set of
	constraints in one constraint.  A proof can be [Direct], meaning that the
	provided linear combinations yields the desired contraint.  When proving
	inclusion in an equality, it might be useful to split it into two inequalities
	(see {! Cs.split}) and provide a proof for each of them ([SplitEq f1 f2]).
	The implementation assumes that the equality being proved is the constraint
	resulting from the linear combination described by [f1], with the comparison
	sign changed to [Cs.Eq]. *)
	type cstr_t
	=	| Direct of frag_t
		| SplitEq of frag_t * frag_t

	(** Pretty-print a certificate for one constraint. *)
	val prC: cstr_t -> string

	(** [isEqC ce ce'] returns [true] if [ce] and [ce'] are the same proof argument.
	Linear combination equality is check with [isEqF]. *)
	val isEqC: cstr_t -> cstr_t -> bool

	(** [cstrFromTriv i f] builds an inclusion certificate for constraint [i]
	from a linear combination yielding a trivial constraint [f]. *)
	val cstrFromTriv: int -> frag_t -> cstr_t

	(** [cstrInv i f] takes a linear combination describing the rewriting
	of a constraint [c], identified by [i] and yielding a constraint [c'];
	it then builds the rewriting of [c'] (identified by [i]) yielding [c]. *)
	val cstrInv: int -> frag_t -> cstr_t

	(** A partial certificate as a list of proofs for some of the output constraints.
	This type lacks integration in the module;
	this is mostly of notational shortcut for now. *)
	type partialT
	= (int * cstr_t) list

	(** Pretty-printer for values of type [partialT]. *)
	val partialPr: partialT -> string

	(** Equality test for values of type [partialT].
	Equality on values of types [cstr_t] is tested with [isEqC].
	Order is irrelevant. *)
	val partialEq: partialT -> partialT -> bool

	(** This type expresses a strategy for proving the inclusion of [p1] in [p2]. 
	The two possibilities are showing that [p1] is [Empty] (i.e. bottom) or that
	there exists, for each constraint [c] of [p2], a linear combination of the
	constraints [p1] yielding [c]. The latter corresponds to an [Implies] certificate.
	Additionally, the possibility is left to defined auxiliary constraints to
	be used in a proof with [Bind]. When using it, care should be taken to use a
	fresh identifier (i.e. no already used in [p1]).

	{e Remarks.} The [Cs.t] is not really useful and should be remove.
	[Bind] is not a really good construction, judging by the amount of code it
	takes to use it in {! Pol}. *)
	type t
	=	| Empty of frag_t
		| Implies of (int * cstr_t) list
		| Bind of (int * Cs.t * cstr_t * t)

	(** Pretty-print an inclusion certificate. *)
	val pr: t -> string

	(** Test equality of two certificates.
	[frag_t] equality is checked using [isEqF].
	Equality for [(int * cstr_t) list] is checked using [partialEq].
	The order of the [Bind] constructs matters. *)
	val isEq: t -> t -> bool

	(** This type captures the arguments of correctness of one contraint [c] of
	the result [p] of the convex hull of two polyhedra [p1] and [p2].
	The correctness condition on [p] states that it should include in
	both [p1] and [p2].  An element of type [joinEltT] provides a way to
	prove that [p1] is included in [c] as its [arg1] field, and a way to
	prove that [p2] is included in [c] as its [arg2] field.
	Last, the constraint [c] of [p] is identified by [id]. *)
	type joinEltT
	= {id: int; arg1: cstr_t; arg2: cstr_t}
	
	val joinElt_to_string : joinEltT -> string
	
	(** Get the contents of field [id] of a [joinEltT]. *)
	val getId: joinEltT -> int

	(** Get the contents of field [arg1] of a [joinEltT]. *)
	val getArg1: joinEltT -> cstr_t

	(** Get the contents of field [arg2] of a [joinEltT]. *)
	val getArg2: joinEltT -> cstr_t

	(** This type captures the arguments of correctness of the result [p]
	of the convex hull of two polyhedra [p1] and [p2]. It is the collection
	of correctness arguments for all of its constituting constraints. *)
	type joinT = joinEltT list
	
	val join_to_string : joinT -> string
	
	(** The proof argument which justifies an assignment.
	[AsgInv] contains the necessary information for the case of
	an invertible operation (x := f(x)), while [AsgNInv] containts
	inversion for a non-invertible operation (e.g. x := 3). *)
	type asgT
	=	| AsgInv of (int * t) list * t
		| AsgNInv of t * (int * t) list
end

module Cert (Cs : Cstr.Rat.Type) = struct 
	module Cs = Cs
	type frag_t = (int * Cs.Vec.Coeff.t) list

	let cmp (i1,_) (i2,_) =
		match Pervasives.compare i1 i2 with
		| 0 -> invalid_arg "Cert.cmp"
		| n -> n

	let sort a = List.sort cmp a

	let prF: frag_t -> string
	= function
		| [] -> "empty"
		| _::_ as l ->
			let pr (i, n) = Printf.sprintf "%s*(%i)" (Cs.Vec.Coeff.to_string n) i in
			String.concat " + " (List.map pr l)

	let isEqF (f1: frag_t) (f2: frag_t): bool =
		List.length f1 = List.length f2 &&
		List.for_all2 (fun (i1, n1) (i2, n2) -> i1 = i2 && Cs.Vec.Coeff.cmp n1 n2 = 0)
			(sort f1) (sort f2)

	let mapF (fn: Cs.Vec.Coeff.t -> Cs.Vec.Coeff.t) (f : frag_t) =
		List.map (fun (i, n) -> (i, fn n)) f

	type cstr_t =
	| Direct of frag_t
	| SplitEq of frag_t * frag_t

	let prC =
		function
		| Direct f -> "Direct:\n\t" ^ (prF f)
		| SplitEq (f1, f2) -> "SplitEq:\n\t" ^ (prF f1) ^ "\n\t" ^ (prF f2)

	let isEqC: cstr_t -> cstr_t -> bool
	= fun ce ce' ->
		match ce, ce' with
		| Direct f, Direct f' -> isEqF f f'
		| SplitEq (f1, f2), SplitEq (f1', f2') -> isEqF f1 f1' && isEqF f2 f2'
		| Direct _, SplitEq (_, _)
		| SplitEq (_, _), Direct _ -> false

	let cstrFromTriv: int -> frag_t -> cstr_t
	= fun i f ->
		let (f1, f2) = List.partition (fun (i', _) -> i = i') f in
		match f1 with
		| [_, n] -> Direct (mapF (fun n' -> Cs.Vec.Coeff.div n' (Cs.Vec.Coeff.neg n)) f2)
		| _ -> Pervasives.failwith "Cert.cstrFromTriv"

	let cstrInv: int -> frag_t -> cstr_t
	= fun i f ->
		let (f1, f2) = List.partition (fun (i', _) -> i = i') f in
		match f1 with
		| [_, n] ->
			let invn = Cs.Vec.Coeff.inv n in
			let op = fun n' -> Cs.Vec.Coeff.mul (Cs.Vec.Coeff.neg invn) n' in
			Direct ((i, invn)::(mapF op f2))
		| _ -> Pervasives.failwith "Cert.cstrInv"

	type partialT
	= (int * cstr_t) list

	let partialPr: partialT -> string
	= fun l ->
		let l' = List.map (fun (i, ce) -> Printf.sprintf "%i: %s" i (prC ce)) l in
		String.concat "\n" l'

	let partialEq: partialT -> partialT -> bool
	=	let cmp = fun (i, _) (i', _) -> Pervasives.compare i i' in
		let eq1 = fun (i, ce) (i', ce') -> i = i' && isEqC ce ce' in
		fun l1 l2 ->
			let l1' = List.sort cmp l1 in
			let l2' = List.sort cmp l2 in
			if List.length l1' = List.length l2'
			then List.for_all2  eq1 l1' l2'
			else false

	type joinEltT = {
		id: int;
		arg1: cstr_t;
		arg2: cstr_t
	}
	
	let joinElt_to_string : joinEltT -> string
		= fun elt ->
		Printf.sprintf "id = %i, arg1 = %s, arg2 = %s"
			elt.id
			(prC elt.arg1)
			(prC elt.arg2)
	
	let getId: joinEltT -> int
	= fun j -> j.id

	let getArg1: joinEltT -> cstr_t
	= fun j -> j.arg1

	let getArg2: joinEltT -> cstr_t
	= fun j -> j.arg2

	type joinT = joinEltT list
	
	let join_to_string : joinT -> string
		= fun join ->
		Misc.list_to_string joinElt_to_string join "\n"
		 
	type t =
	| Empty of frag_t
	| Implies of (int * cstr_t) list
	| Bind of (int * Cs.t * cstr_t * t)

	let rec isEq: t -> t -> bool
	= fun ce ce' ->
		match ce, ce' with
		| Empty f, Empty f' -> isEqF f f'
		| Implies l, Implies l' -> partialEq l l'
		| Bind (i, _, ce1, ce2), Bind (i', _, ce1', ce2') ->
			i = i' && isEqC ce1 ce1' && isEq ce2 ce2'
		| _, _ -> false

	type asgT
	= AsgInv of (int * t) list * t | AsgNInv of t * (int * t) list

	type cert_t = t

	let mkF (init: (int * Cs.Vec.Coeff.t) list) =
		if List.for_all (fun (_, a) -> Cs.Vec.Coeff.cmpz a <> 0) init then
			try
				sort init
			with
			| Invalid_argument "Cert.cmp" -> invalid_arg "Cert.mkF"
		else
			invalid_arg "Cert.mkF"

	let renameF: int -> int -> frag_t -> frag_t
	= fun idOld idNew f ->
		let rename (i, n) =
			if idOld = i then
				(idNew, n)
			else
				(i, n)
		in
		List.map rename f

	let mergeF (l0: (frag_t * Cs.Vec.Coeff.t) list): frag_t =
		let l = List.map
			(fun (f, n) ->
				if Cs.Vec.Coeff.cmpz n = 0 then []
				else
					List.map (fun (i, a) -> (i, Cs.Vec.Coeff.mul n a)) f) l0
		in
		let rec merge2 f1 f2 =
			match f1, f2 with
			| [], t | t, [] -> t
			| (i1, n1)::t1, (i2, n2)::t2 ->
				if i1 < i2 then
					(i1, n1)::(merge2 t1 f2)
				else if i1 > i2 then
					(i2, n2)::(merge2 f1 t2)
				else
					let n = Cs.Vec.Coeff.add n1 n2 in
		                             let t = merge2 t1 t2 in
					if Cs.Vec.Coeff.cmpz n = 0 then t else (i1, n)::t
		in
		List.fold_left merge2 [] (List.map sort l)

	let rec pr =
		function
		| Empty f -> "Empty: " ^ (prF f)
		| Bind (id, _, ce1, ce2) -> Printf.sprintf "Bind %i: %s\nin\n%s" id (prC ce1) (pr ce2)
		| Implies ce0 ->
			match ce0 with
			| [] -> "Implies: empty"
			| h::t ->
				let p (i, c) = "(" ^ (string_of_int i) ^ "): " ^ (prC c) in
				List.fold_left (fun s c -> s ^ "\n" ^ (p c)) (p h) t
end
*)
