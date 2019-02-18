type cmpT = Cstr_type.cmpT

module Cs = Cstr.Rat
module Poly = Poly.Make(Vector.Rat)

type t = {
	typ : cmpT;
	p: Poly.t;
}

let to_string : t -> string
	= fun cp ->
	Printf.sprintf "%s %s 0"
		(Poly.to_string cp.p)
		(match cp.typ with
		| Cstr_type.Eq -> " = "
		| Cstr_type.Le -> " <= "
		| Cstr_type.Lt -> " < ")

let mk : cmpT -> Poly.t -> t
	= fun typ p ->
	{p = p ; typ = typ}

let eq : Poly.t -> t
	= fun p ->
	{p = p ; typ = Cstr_type.Eq}

let le : Poly.t -> t
	= fun p ->
	{p = p ; typ = Cstr_type.Le}

let lt : Poly.t -> t
	= fun p ->
	{p = p ; typ = Cstr_type.Lt}

let compl : t -> t
	= fun c ->
	match c.typ with
	| Cstr_type.Eq -> invalid_arg "CstrPoly.compl"
	| Cstr_type.Le -> { typ = Cstr_type.Lt; p = Poly.neg c.p }
	| Cstr_type.Lt -> { typ = Cstr_type.Le; p = Poly.neg c.p }

let empty : t
	= eq Poly.z

let equal: t -> t -> bool
	= fun cp1 cp2 ->
	cp1.typ = cp2.typ && Poly.equal cp1.p cp2.p

let cmp : t -> t -> int
	= fun cp1 cp2 ->
	match (cp1.typ,cp2.typ) with
	| (Cstr_type.Eq,Cstr_type.Le) | (Cstr_type.Le,Cstr_type.Lt) | (Cstr_type.Eq,Cstr_type.Lt) -> -1
	| (Cstr_type.Le,Cstr_type.Eq) | (Cstr_type.Lt,Cstr_type.Le) | (Cstr_type.Lt,Cstr_type.Eq) -> 1
	| (_,_) -> Poly.compare cp1.p cp2.p

let compare = cmp

let toCstr : t -> Cs.t
	= fun cp ->
	let (vec,cste) = Poly.toCstr cp.p in
	Cs.mk2 cp.typ vec (Scalar.Rat.neg cste)

let ofCstr : Cs.t -> t
	= fun c ->
	let p = Poly.ofCstr c.Cs.v (Scalar.Rat.neg c.Cs.c) in
	mk c.Cs.typ p

let partition_affine : t list -> (Cs.t list * t list)
	= fun polys ->
	let (affs, ps) = List.partition (fun p -> Poly.is_linear p.p) polys in
	(List.map toCstr affs, ps)
