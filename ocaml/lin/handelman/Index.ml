(** This module type defines the type of indexes.
	An index is a plain vector of coefficients.
	It is given as a functor that depends on a type of coefficients.
*)

(** This module defines the type of Scalar used as coefficients in Indexes *)
module type Scalar = sig
	type t
	val z : t
	val u : t
	val negU: t
	val add : t -> t -> t
	val mul: t -> t -> t
	val to_string: t -> string
	val of_string: string -> t
	val le : t -> t -> bool
	val lt : t -> t -> bool
	val equal: t -> t -> bool
end

(** Functor of Index *)
module Make (Coeff : Scalar) = struct

	(** Type of coefficients *)
	type coeff = Coeff.t

	(** Type of Index *)
	type t = coeff list

	(** [init n] returns an index of length [n] filled with zeros. *)
	let init : int -> t
		= fun len ->
		Misc.init_list len (fun _ -> Coeff.z)

	(** [len ind] returns the length of index [ind]. *)
	let len : t -> int
		= fun i ->
		List.length i

	(** [is_nonnegative ind] returns true if every element of index [ind] is nonnegative. *)
	let is_nonnegative : t -> bool
		= fun i ->
		List.for_all (fun j -> Coeff.le Coeff.z j) i

	(** [well_formed ind] returns [true] if ind is well formed, [false] otherwise. *)
	let well_formed : t -> bool
		= fun i -> is_nonnegative i

	(** [mk l] builds an index from the coefficient list [l].
		@raise Invalid_argument if [l] contains negative elements. *)
	let mk : coeff list -> t
		= fun l ->
		if is_nonnegative l then l
		else Stdlib.invalid_arg "IndexC.mk : coefficients are not nonnegative"

	(** [unitary i len] returns an index of length [len], whose value is 1 at index [i] and 0 otherwise. *)
	let unitary : int -> int -> t
		= fun i len ->
		Misc.init_list len (fun j ->
			if i = j then Coeff.u else Coeff.z
		)

	(** [sum ind] returns the sum of all elements of [ind]. *)
	let sum : t -> Coeff.t
		= fun i -> List.fold_left Coeff.add Coeff.z i

	(** [set ind i c] returns an index equal to [ind], with the [i]th coefficient equals to [c]. *)
	let set : t -> int -> Coeff.t -> t
		= fun ind j c ->
		List.mapi (fun i v -> if i = j then c else v) ind

	(** [get ind i] returns the [i]th coefficient of index [ind]. *)
	let get : t -> int -> Coeff.t
		= fun i j ->
		List.nth i j

	(** [incr ind i] returns and index equal to [ind] such that [i]th coefficient equals to [ind(i)]+1. *)
	let incr : t -> int -> t
		= fun i j ->
		List.mapi (fun k v -> if k = j then Coeff.add v Coeff.u else v) i

	(** [decr ind i] returns and index equal to [ind] such that [i]th coefficient equals to [ind(i)]-1. *)
	let decr : t -> int -> t
		= fun i j ->
		List.mapi (fun k v -> if k = j then Coeff.add v Coeff.negU else v) i

	(** [add ind1 ind2] returns an index such that the ith coefficient equals to [ind1(i)] + [ind2(i)]. *)
	let add : t -> t -> t
		= fun i1 i2 ->
		List.map2 Coeff.add i1 i2

	(** [sumI il] computes the sum of all elements in [il].
		@raise Invalid_argument if [l] is empty. *)
	let sumI : t list -> t
		= fun il ->
		try
			let dim = len (List.hd il) in
			List.fold_left add (init dim) il
		with _ -> Stdlib.invalid_arg "Index.sum : empty input list"

	(** [sub ind1 ind2] returns an index such that the ith coefficient equals to [ind1(i)] - [ind2(i)]. *)
	let sub : t -> t -> t
		= fun i1 i2 ->
		List.map2 (fun j1 j2 ->
			Coeff.add j1 (Coeff.mul j2 Coeff.negU)
		) i1 i2

	(** [equal ind1 ind2] returns [true] if for all i [ind1(i)] = [ind2(i)], [false] otherwise. *)
	let equal : t -> t -> bool
		= fun i1 i2 ->
		List.for_all2 Coeff.equal i1 i2

	(** [is_null ind] returns [true] if for all i [ind(i)] = 0, [false] otherwise. *)
	let is_null : t -> bool
		= fun i ->
		List.for_all (fun j -> Coeff.equal j Coeff.z) i

	(** [first_positive ind] returns the place of the first strictly positive coefficient.
	@raise Not_found if there is no such coefficient. *)
	let first_positive : t -> int
		= fun id ->
		let rec f i = function
		| [] -> Stdlib.raise Not_found
		| x :: l ->
			if Coeff.lt Coeff.z x
			then i
			else f (i+1) l
		in f 0 id

	let to_string : t -> string
		= fun i ->
		List.map (fun j -> Coeff.to_string j) i
		|> String.concat ";"
		|> Printf.sprintf "(%s)"

	(** [compare ind1 ind2] returns a positive integer if [ind1] > [ind2] ({i w.r.t.} lexicographic order), a negative one if [ind1] < [ind2], 0 otherwise. *)
	let rec compare : t -> t -> int
		= fun i1 i2 ->
		match (i1,i2) with
			| ([],[]) -> 0
			| (_,[]) -> 1
			| ([],_) -> -1
			| (j1::tl1, j2::tl2) -> let x = Stdlib.compare j1 j2 in
			match x with
				| 0 -> compare tl1 tl2
				| _ -> x

	(** [le ind1 ind2] returns [true] if [ind1] <= [ind2] ({i w.r.t} elementwise order), [false] otherwise. *)
	let le : t -> t -> bool
		= fun i1 i2 ->
		List.for_all2 Coeff.le i1 i2

	(** [le_nl ind1 ind2] returns [true] if [ind1] <= [ind2] ({i w.r.t} elementwise order), [false] otherwise.
	It ignores coefficients that are equal to one. *)
	let le_nl : t -> t -> bool
		= fun i1 i2 ->
		List.for_all2 (fun j1 j2 ->
			Coeff.le j1 Coeff.u || Coeff.le j1 j2
		) i1 i2

	(** [is_unitary ind] returns [true] if [ind] contains a single one, and every other coefficient is null, [false] otherwise. *)
	let is_unitary : t -> bool
		= fun i ->
		let rec f b i =
			match i with
			| [] -> b
			| x :: l ->
				if Coeff.equal x Coeff.z
				then f b l
				else if Coeff.equal x Coeff.u
				then if b
					then false
					else f true l
				else false
		in
		f false i

	(** [one_coeff_nn ind] returns [true] if [ind] contains a single nonnull coefficient, [false] otherwise. *)
	let one_coeff_nn : t -> bool
		= fun i ->
		let rec f b i =
			match i with
			| [] -> b
			| x :: l ->
				if Coeff.equal x Coeff.z
				then f b l
				else if b
					then false
					else f true l
		in
		f false i

	let data : t -> coeff list
		= fun ind -> ind

end

(** Type of integer scalars. *)
module ScalarInt = struct
	type t = int
	let z = 0
	let u = 1
	let negU = -1
	let add = (+)
	let mul = ( * )
	let le = ( <= )
	let lt = ( < )
	let to_string = string_of_int
	let of_string = int_of_string
	let equal = ( = )
end

(** Type of indexes where coefficients are integers. *)
module Int = struct
	include Make (ScalarInt)

	(** [value ind] returns the euclidian norm of index [ind]. *)
	let (value : t -> float)
		= fun i -> sqrt (List.fold_left (fun f j -> f +. float_of_int (j*j)) 0.0 i)
end

(** Type of indexes where coefficient type is {!type:SxPoly.ScalarQ.t}. *)
module Rat = struct
	include Make (struct include Scalar.Rat let of_string = Q.of_string end)
end
