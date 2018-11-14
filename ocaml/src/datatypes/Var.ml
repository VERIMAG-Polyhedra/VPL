open Var_type

module type Type = Type

module Positive = struct
	type t = positive

	let name = "Positive"

	let toInt: t -> int
	= fun p0 ->
		let inc i =
			let msb = Pervasives.max_int - (Pervasives.max_int lsr 1) in
			if i = msb then (* overflow *)
				Pervasives.invalid_arg "Var.toInt"
			else
				i lsl 1
		in
		let rec fn i msb = function
			| XH -> msb + i
			| XO p -> fn i (inc msb) p
			| XI p -> fn (msb + i) (inc msb) p
		in
		fn 0 1 p0

	let fromInt: int -> t
	= fun i0 ->
		let rec _fromInt i =
			let iMasked = i land 1 in
			if i = iMasked then
				XH
			else
				let tail = _fromInt (i lsr 1) in
				if iMasked = 1 then
					XI tail
				else
					XO tail
		in
		if i0 > 0 then
			_fromInt i0
		else
			Pervasives.invalid_arg "Var.fromInt"

	let to_string': string -> t -> string
		= fun s p -> s ^ (Pervasives.string_of_int (toInt p))

	let to_string : t -> string
		= to_string' "v"

	let plp_print : t -> string
		= to_string' ""

	(** [next v] gives the path to the next variable in a breadth-first search in a tree.
Next of [... XO XH] is [... XI XH],
next of [... XO (XI XH)] is [... XI (XO XH)] and
next of [XI (XI XH)] is [XO (XO (XO XH))]. *)
	let next (bp: t): t =
		let rec next_rec (bp': t): t * bool =
			match bp' with
			  XH -> (XH, true)
			| XI tail -> (
				match next_rec tail with
				  (tail', true) -> (XO tail', true)
				| (tail', false) -> (XI tail', false))
			| XO tail -> (
				match next_rec tail with
				  (tail', true) -> (XI tail', false)
				| (tail', false) -> (XO tail', false))
		in
		let (res, overflow) = next_rec bp in
		if overflow then XO res else res

	let cmp: t -> t -> int
	= fun x00 x01 ->
		let rec _cmp x0 x1 dec res =
			match x0, x1 with
			| XO tl0, XO tl1 -> _cmp tl0 tl1 dec res
			| XI tl0, XI tl1 -> _cmp tl0 tl1 dec res
			| XI tl0, XO tl1 -> _cmp tl0 tl1 true (if dec then res else 1)
			| XO tl0, XI tl1 -> _cmp tl0 tl1 true (if dec then res else -1)
			| XH, XO _ | XH, XI _ -> -1
			| XO _, XH  | XI _, XH -> 1
			| XH, XH -> res
		in
		_cmp x00 x01 false 0

	let equal x y = cmp x y = 0

	module Set
	  = Set.Make (struct type varT = t type t = varT let compare = cmp end)

	let horizon : Set.t -> t
	  = fun s -> if Set.cardinal s < 1 then XH else next (Set.max_elt s)

	let fromLeft : t -> t
		= fun x -> XO x

	let fromRight : t -> t
		= fun x -> XI x

	let u = XH

	let toPos x = x
	let fromPos x = x

	let of_string : string -> t
		= fun s ->
		int_of_string s
		|> fromInt

	let of_prefixed_string : string -> t
	  = fun s ->
	  let s' = String.sub s 1 (String.length s - 1) in
	  try of_string s'
	  with Failure _ ->
		 let e = Printf.sprintf "SxPoly.VariablesInt.of_prefixed_string: s = %s; s' = %s" s s' in
		 Pervasives.invalid_arg e

	let max : t list -> t
		= fun l ->
		Misc.max cmp l
end

module Int = struct
	type t = int

	let name = "Int"

	let z = 0

	let equal x y = x = y

	let cmp x y = if x < y then -1 else if x = y then 0 else 1

	let equal x y = cmp x y = 0

	let of_string = int_of_string

	let of_prefixed_string : string -> t
	  = fun s ->
	  let s' = String.sub s 1 (String.length s - 1) in
	  try of_string s'
	  with Failure _ ->
		 let e = Printf.sprintf "SxPoly.VariablesInt.of_prefixed_string: s = %s; s' = %s" s s' in
		 Pervasives.invalid_arg e

	let toInt x = x

	let fromInt x =
		if x <= 0
		then Pervasives.invalid_arg "Var.fromInt"
		else x

	let toPos x = Positive.fromInt x
	let fromPos x = Positive.toInt x


	let to_string = string_of_int

	let to_string': string -> t -> string
	= fun s p -> s ^ (Pervasives.string_of_int p)

	let plp_print = string_of_int

	let of_prefixed_string : string -> t
	  = fun s ->
	  let s' = String.sub s 1 (String.length s - 1) in
	  try Pervasives.int_of_string s'
	  with Failure _ ->
		 let e = Printf.sprintf "SxPoly.VariablesInt.of_prefixed_string: s = %s; s' = %s" s s' in

		 Pervasives.invalid_arg e

	let fromLeft : t -> t
		= fun v ->
		2*v

	let fromRight : t -> t
		= fun v ->
		2*v+1

	(* XXX: ou -1? *)
	let u : t
		= 1

	(** [next v] returns [v+1]. *)
	let next : t -> t
		= fun v ->
		v + 1

	module Set
	  = Set.Make (struct type varT = t type t = varT let compare = cmp end)

	let horizon : Set.t -> t
	  = fun s -> if Set.cardinal s < 1 then u else next (Set.max_elt s)

	let max : t list -> t
		= fun l ->
		Misc.max cmp l
end

module String = struct
	type t = string

	let name = "String"

	let z = "0"

	let equal x y = x = y

	let cmp = Pervasives.compare

	let equal x y = cmp x y = 0

	let of_string x = x

	let of_prefixed_string x = x

	let toInt x = int_of_string x

	let fromInt x =
		if x <= 0
		then Pervasives.invalid_arg "Var.fromInt"
		else "x" ^ (string_of_int x)

	let toPos _ = Pervasives.failwith "Var.String.toPos: unimplemented"
	let fromPos _ = Pervasives.failwith "Var.String.fromPos: unimplemented"

	let to_string x = x

	let to_string': string -> t -> string
	= fun s p -> s ^ p

	let plp_print = to_string

	let fromLeft _ = Pervasives.failwith "Var.String.fromLeft : uninplemented"

	let fromRight _ = Pervasives.failwith "Var.String.fromRight : uninplemented"

	(* XXX: ou -1? *)
	let u : t
		= "1"

	(** [next v] returns [v+1]. *)
	let next : t -> t
		= fun x -> toInt x |> (+) 1 |> fromInt

	module Set
	  = Set.Make (struct type varT = t type t = varT let compare = cmp end)

	let horizon : Set.t -> t
	  = fun s -> if Set.cardinal s < 1 then u else next (Set.max_elt s)

	let max : t list -> t
		= fun l ->
		Misc.max cmp l
end
