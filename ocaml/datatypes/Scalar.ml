open Scalar_type

module type Type = Type

module Float = struct
	type t = float

	let name = "Float"

	let delta = 0.00000000001

	let to_string x =
		if x < 0.001
		then string_of_float x
		else Printf.sprintf "%.3f" x

	let plp_print : t -> string
		= fun v ->
		(Q.of_float v |> Q.to_string) ^ " 0"

	let z = float 0
	let u = float 1
	let negU = float (-1)

	let mul = ( *. )
	let pow : t -> int -> t
		= fun x exp ->
		let rec pow_rec r i =
			if i >= exp
			then r
			else pow_rec (mul r x) (i+1)
		in
		pow_rec u 0

	let div  = ( /. )
	let add = ( +. )
	let sub = ( -. )
	let neg = mul negU
	let abs x = if x > z then x else mul negU x
	let inv x = div u x
	let cmp = Stdlib.compare
	let le  = (<=)
	let lt = (<)
	let cmpz = cmp z
	let equal c1 c2 = (cmp c1 c2 = 0)
	let equalApprox c1 c2 = le (sub c1 c2 |> abs) delta
	let isZ x = equal z x

	let of_string = float_of_string

	let ofQ : Q.t -> t
		= fun q ->
		(float_of_string (q.Q.num |> Z.to_string)) /. (float_of_string (q.Q.den |> Z.to_string))

	let toQ : t -> Q.t
		= fun n ->
		Q.of_float n

	let well_formed x = not (x = nan) && not (x = infinity) && not (x = neg_infinity)
	let well_formed_nonnull x = not (equal x z) && well_formed x

	let to_float t = t
	let of_float t = t

	let mulr : Q.t -> t -> t
		= fun q f ->
		mul (ofQ q) f

	let divr : t -> Q.t -> t
		= fun f q ->
		div f (ofQ q)

	let mk (den: int) (num: int) = float(num) /. float(den)
	let of_int (num : int) = float(num)

	let to_int f round =
		let i = int_of_float f in
		if (match round with
			| Up -> of_int i < f
			| Down -> of_int i > f)
		then match round with
			| Up -> Some (i + 1)
			| Down -> Some(i - 1)
		else Some i

    let gcd _ _ = failwith "Float.gcd: unimplemented"
    let toZ _ = failwith "Float.toZ: unimplemented"
    let ofZ n d = div (Z.to_string n |> of_string) (Z.to_string d |> of_string)
end

(** Rationals are used everywhere in the VPL.
	The implementation uses the ZArith front-end to GMP. *)
module Rat = struct
	type t = Q.t

	let name = "Rat"

	let z = Q.zero
	let u = Q.one
	let negU = Q.minus_one

	let cmp = Q.compare
	let le = Q.leq
	let lt = Q.lt
	let cmpz q = -(Q.sign q)

	let isZ : t -> bool
		= fun n -> cmpz n = 0

	let abs = Q.abs
	let neg = Q.neg
	let inv = Q.inv

	let add = Q.add
	let sub = Q.sub
	let mul = Q.mul
	let div = Q.div
	let pow : t -> int -> t
		= fun x exp ->
		let rec pow_rec r i =
			if i >= exp
			then r
			else pow_rec (mul r x) (i+1)
		in
		pow_rec u 0

	let ofQ : Q.t -> t = fun n -> n
	let toQ : Q.t -> t = fun n -> n

	let mk (den: int) (num: int) = Q.of_ints num den
	let of_int (num : int) = Q.of_ints num 1

	let of_float = Q.of_float

	let to_float : t -> float
		= fun v ->
        Z.to_float (Q.num v) /. Z.to_float (Q.den v)

	let to_string = Q.to_string
	let of_string = Q.of_string

	let plp_print : t -> string
		= fun v ->
		(to_string v) ^ " 0"

	let equal c1 c2 = (cmp c1 c2 = 0)

	let delta = (of_float Float.delta)

	let equalApprox c1 c2 = cmp (sub c1 c2 |> abs) delta <= 0

	let well_formed_nonnull x = Q.classify x = Q.NZERO

	let well_formed x =
		let x_type = Q.classify x in
			x_type <> Q.INF && x_type <> Q.MINF && x_type <> Q.UNDEF

	(** Some operations can yield infinity (e.g. 1/0) or undefined results (0/0).
	[isReal n] returns true if [n] is neither infinite nor undefined. *)
	let isReal = Q.is_real

	(** [isInt n] returns true if [n] is a relative integer
	that is if its denominator is equal to [u]. *)
	let isInt n = Z.equal (Q.den n) Z.one

	(** The type of relative integers that rationals are made out of, and operations on them. *)
	module Z = struct
		type t = Z.t

		let z = Z.zero
		let u = Z.one

		let add : t -> t -> t
		= Z.add

		let sub : t -> t -> t
		= Z.sub

		let mul : t -> t -> t
		= Z.mul

		(** [div a b] yields the result of integer division [a]/[b].
		The result obeys the rule of signs.  Division by zero has
		undefined behaviour. *)
		let div : t -> t -> t
		= Z.div

		(** [rem a b] yields the remainder of integer division [a]/[b].
		The result has the sign of [a].  [rem a 0] has undefined behaviour. *)
		let rem : t -> t -> t
		= Z.rem

		let cmp = Z.compare
		let equal = Z.equal
		let neg = Z.neg
		let gcd = Z.gcd
		let lAnd = Z.logand
		let orL = Z.logor
		let shiftL = Z.shift_left
		let shiftR = Z.shift_right

		let mk = Z.of_int

		(** build a value of type [t] from its string representation in base 10 *)
		let ofString : string -> t
		= Z.of_string

		let toInt = Z.to_int
		let pr = Z.to_string
	end

	let gcd (nGcd, dGcd) a =
		((if Z.equal Z.u nGcd then
			Z.u
		else
			Z.gcd nGcd (Q.num a)),
		(if Z.equal Z.u dGcd then
			Z.u
		else
			Z.gcd dGcd (Q.den a)))

	(** [toZ n] returns the numerator and denominator of [n], in that order. *)
	let toZ a = (Q.num a, Q.den a)

	(** [ofZ n d] builds a rational of numerator [n] and denominator [d]. *)
	let ofZ num den = Q.make num den

	let mulr = mul

	let divr : t -> Q.t -> t
		= fun f q ->
		div f (ofQ q)

	let to_int q round =
		try
			let i = let (num,den) = toZ q in
				let num = Z.toInt num
				and den = Z.toInt den in
				num / den
			in
			if (match round with
				| Up -> lt (of_int i) q
				| Down -> lt q (of_int i))
			then match round with
				| Up -> Some (i + 1)
				| Down -> Some (i - 1)
			else Some i
		with _ -> None
end

module Int = struct
	type t = Z.t

	let name = "Z"

	let z = Z.zero
	let u = Z.one
	let negU = Z.minus_one

	let cmp = Z.compare
	let le = Z.leq
	let lt = Z.lt
	let cmpz q = -(Z.sign q)

	let isZ : t -> bool
		= fun n -> cmpz n = 0

	let abs = Z.abs
	let neg = Z.neg
	let inv _ = Stdlib.failwith "Scalar.Int.inv"

	let add = Z.add
	let sub = Z.sub
	let mul = Z.mul
	let div = Z.div
	let pow : t -> int -> t
		= fun x exp ->
		let rec pow_rec r i =
			if i >= exp
			then r
			else pow_rec (mul r x) (i+1)
		in
		pow_rec u 0

	let ofQ _ = Stdlib.failwith "Scalar.Int.ofQ"
	let toQ n = Rat.ofZ n u

    let gcd _ _ = failwith "unimplemented"
    let toZ _ = failwith "unimplemented"
    let ofZ _ _ = failwith "unimplemented"

	let of_int : int -> t
		= fun i ->
		Z.of_int i

	let mk: int -> int -> t
		= fun den nom ->
		Z.div (of_int nom) (of_int den)

	let to_int x round =
		try
			let i = Z.to_int x in
			if (match round with
				| Up -> lt (of_int i) x
				| Down -> lt x (of_int i))
			then match round with
				| Up -> Some (i + 1)
				| Down -> Some (i - 1)
			else Some i
		with _ -> None

	let of_float = Z.of_float

	let to_float : t -> float
		= fun v ->
		Z.to_int v
			|> float_of_int

	let to_string = Z.to_string
	let of_string = Z.of_string

	let plp_print : t -> string
		= fun v ->
		(to_string v) ^ " 0"

	let equal c1 c2 = (cmp c1 c2 = 0)

	let delta = z

	let equalApprox c1 c2 = cmp (sub c1 c2 |> abs) delta <= 0

	let well_formed_nonnull n = not (isZ n)

	let well_formed _ = true

	(** Multiplication by a rational. *)
	let mulr : Q.t -> t -> t
		= fun _ _ ->
		Stdlib.failwith "Scalar.Int.mulr"

	let divr : t -> Q.t -> t
		= fun _ _ ->
		Stdlib.failwith "Scalar.Int.divr"
end

type symbolic = { v: Q.t; d: Q.t }

(** Symbolic values have the form [a + b.delta], where [a] and [b] are rationals, and [delta] is symbolic.
They are used in module {!module:Splx} and {!module:Opt} to represent values with strict inequalities. *)
module Symbolic = struct

	type t = symbolic

	let name = "Symbolic"

	let get_d (x : t) = x.d

    let get_v (x : t) = x.v

	let pdelta n = { v = n; d = Rat.u }

	let ndelta n = { v = n; d = Rat.negU }

	let z = { v = Rat.z; d = Rat.z }

	let cmp v1 v2 =
		match Rat.cmp v1.v v2.v with
		| 0 -> Rat.cmp v1.d v2.d
		| n -> n

	let le v1 v2 = cmp v1 v2 <= 0

	let lt v1 v2 = cmp v1 v2 < 0

	let hasDelta v = (Rat.cmpz v.d <> 0)

	let ofQ n = { v = n; d = Q.zero }

	let toQ : t -> Q.t
		= fun v ->
		if hasDelta v
		then Q.add
				(get_v v)
				(Q.mul Rat.delta (get_d v))
		else get_v v

	let mulr n v = {
		v = Rat.mul v.v n;
		d = Rat.mul v.d n
	}

	let divr v n = {
		v = Rat.div v.v n;
		d = Rat.div v.d n
	}

	let add v1 v2 = {
		v = Rat.add v1.v v2.v;
		d = Rat.add v1.d v2.d
	}

	let sub v1 v2 = {
		v = Rat.sub v1.v v2.v;
		d = Rat.sub v1.d v2.d
	}

	let adddelta v = { v with d = Rat.add v.d Rat.u }

	let subdelta v = { v with d = Rat.sub v.d Rat.u }

	let to_string v = Printf.sprintf "%s + %s.delta" (Rat.to_string v.v) (Rat.to_string v.d)

	let cmpz = (cmp z)

	let isZ x = cmpz x = 0

	let negU = ofQ Rat.negU

	let abs x = if cmpz x > 0
		then mulr Rat.negU x
		else x

	let equal c1 c2 = (cmp c1 c2 = 0)

	let delta = pdelta Rat.z

	let equalApprox c1 c2 = cmp (sub c1 c2 |> abs) delta < 0

	let u = ofQ (Rat.u)

	let neg : t -> t
		= fun v ->
		mulr Rat.negU v

	(* On triche en négligeant les delta^2 *)
	let mul : t -> t -> t
		= fun v1 v2 ->
		add
			(add
				(mulr (Rat.mul (get_v v1) (get_d v2))
					(pdelta Rat.z))
				(mulr (Rat.mul (get_v v2) (get_d v1))
				(pdelta Rat.z)))
			(ofQ (Rat.mul (get_v v1) (get_v v2)))

	let pow : t -> int -> t
		= fun x exp ->
		let rec pow_rec r i =
			if i >= exp
			then r
			else pow_rec (mul r x) (i+1)
		in
		pow_rec u 0

	(* On triche en négligeant les delta^2 *)
	let div : t -> t -> t
		= fun v1 v2 ->
		let a1 = get_v v1 and
		a2 = get_v v2 and
		b1 = get_d v1 and
		b2 = get_d v2 in
		let a3 = Rat.div a1 a2 in
		let b3 = Rat.div
			(Rat.sub
				(Rat.mul a3 b2)
				b1)
			(Rat.neg a2) in
		let res = add
	 	(mulr
	 		b3
	 		(pdelta Rat.z))
		 (ofQ a3) in
		 res

	let ofVal : t -> t
		= fun x -> x

	let to_rat : t -> Rat.t
		= fun v ->
		if hasDelta v
		then Rat.add
				(get_v v)
				(Rat.mul Rat.delta (get_d v))
		else get_v v

	let plp_print : t -> string
		= fun v ->
		(Rat.to_string (get_v v)) ^ " " ^ (Rat.to_string (get_d v))

	let inv x = div u x

	let of_float : float -> t
		= fun v ->
		Rat.of_float v
		|> ofQ

	let to_float : t -> float
		= let float_of_val : t -> float
			= fun v ->
			if hasDelta v
			then Float.add
					(get_v v |> Float.ofQ)
					(Float.mul Float.delta (get_d v |> Float.ofQ))
			else get_v v |> Float.ofQ
		in
		fun v ->
		float_of_val v
		|> Float.to_float

	let mk (den: int) (num: int) = ofQ (Rat.mk den num)
	let of_int (num : int) = ofQ (Rat.of_int num)

	let well_formed_nonnull x = Rat.well_formed_nonnull (get_v x) && Rat.well_formed_nonnull (get_d x)

	let well_formed x = Rat.well_formed (get_v x) && Rat.well_formed (get_d x)

	let of_string _ = print_endline "Scalar.Val.of_string : not implemented" ; z

	let to_int _ _ = Stdlib.failwith "Scalar.Symbolic.to_int : not implemented"

    let gcd _ _ = failwith "unimplemented"
    let toZ _ = failwith "unimplemented"
    let ofZ _ _ = failwith "unimplemented"

end
