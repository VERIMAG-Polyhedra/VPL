open Vector_type

module type Type = Type

module Make (Coeff : Scalar.Type) = struct
	type t = Coeff.t Rtree.t

	let name : string = "Rtree with coeff type : " ^ (Coeff.name)

	let cut : t -> Coeff.t -> t -> t
			= fun l n r ->
		if Coeff.cmp Coeff.z n = 0 && l = Rtree.Nil && r = Rtree.Nil then
			Rtree.Nil
		else
			Rtree.Sub (l, n, r)

	let set ve0 va0 n0 =
		let z = (Coeff.cmp Coeff.z n0 = 0) in
		let rec _set ve va =
			match ve, va with
			| Rtree.Nil, Var.XH when z -> Rtree.Nil
			| Rtree.Nil, Var.XH -> Rtree.Sub (Rtree.Nil, n0, Rtree.Nil)

			| Rtree.Sub (Rtree.Nil, _, Rtree.Nil), Var.XH when z -> Rtree.Nil
			| Rtree.Sub (l, _, r), Var.XH -> Rtree.Sub (l, n0, r)

			| Rtree.Nil, Var.XO t -> cut (_set Rtree.Nil t) Coeff.z Rtree.Nil
			| Rtree.Nil, Var.XI t -> cut Rtree.Nil Coeff.z (_set Rtree.Nil t)

			| Rtree.Sub (l, n, r), Var.XO t -> cut (_set l t) n r
			| Rtree.Sub (l, n, r), Var.XI t -> cut l n (_set r t)
		in
		_set ve0 va0

	let mk (i: (Coeff.t * Var.t) list) =
		List.fold_left (fun v (n, var) -> set v var n) Rtree.Nil i

	let rec mul_t : t -> t -> t
		= fun v1 v2 ->
		match (v1, v2) with
		| (Rtree.Nil, Rtree.Nil) -> Rtree.Nil
		| (Rtree.Sub _, Rtree.Nil) -> Rtree.Nil
		| (Rtree.Nil, Rtree.Sub _) -> Rtree.Nil
		| (Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2)) ->
			cut (mul_t l1 l2) (Coeff.mul n1 n2) (mul_t r1 r2)

	let rec add : t -> t -> t
		= fun v1 v2 ->
		match (v1, v2) with
		| (Rtree.Nil, Rtree.Nil) -> Rtree.Nil
		| (Rtree.Sub _, Rtree.Nil) -> v1
		| (Rtree.Nil, Rtree.Sub _) -> v2
		| (Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2)) ->
			cut (add l1 l2) (Coeff.add n1 n2) (add r1 r2)

	let map f v0 =
		let rec _map v =
			match v with
			| Rtree.Nil -> Rtree.Nil
			| Rtree.Sub (l, n, r) -> cut (_map l) (f n) (_map r)
		in
		_map v0

    let eval vec point =
        Rtree.fold
            (fun _ -> Coeff.add)
            Coeff.z
            (mul_t vec point)

	let mulc n v = map (fun v' -> Coeff.mul n v') v

	let divc v c = map (fun n -> Coeff.div n c) v

	let mulr n v = map (fun v' -> Coeff.mulr n v') v

	let divr v q = map (fun n -> Coeff.divr n q) v

	let toList : t -> (Var.t * Coeff.t) list
		=	let rmZeroes : (Var.t * Coeff.t) list -> (Var.t * Coeff.t) list
			= fun l -> List.filter (fun (_,n) -> Coeff.cmp Coeff.z n <> 0) l
			in
			fun v -> rmZeroes (Rtree.toList v)

	let get : t -> Var.t -> Coeff.t
		= fun x v ->
		Rtree.get Coeff.z x v

	let neg : t -> t
		= fun x ->
		map (fun c -> Coeff.mul Coeff.negU c) x

	let sub : t -> t -> t
		= fun v1 v2 ->
		add v1 (neg v2)

	let middle : t -> t -> t
		= fun x x' ->
		add x x'
		|> mulc (Coeff.mk 2 1)

	let rec equal : t -> t -> bool
		= fun p1 p2 ->
		match (p1,p2) with
		| (Rtree.Nil, Rtree.Nil) -> true
		| (Rtree.Sub _, Rtree.Nil) -> false
		| (Rtree.Nil, Rtree.Sub _) -> false
		| (Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2)) ->
			if Coeff.cmp n1 n2 = 0 then
				equal l1 l2 && equal r1 r2
			else
				false

	let rec dot_product : t -> t -> Coeff.t
		= fun v1 v2 ->
		match (v1, v2) with
		| (Rtree.Nil, Rtree.Nil) -> Coeff.z
		| (Rtree.Sub _, Rtree.Nil) -> Coeff.z
		| (Rtree.Nil, Rtree.Sub _) -> Coeff.z
		| (Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2)) ->
			Coeff.add
				(Coeff.mul n1 n2)
				(Coeff.add (dot_product l1 l2) (dot_product r1 r2))

	let isomorph : t -> t -> Coeff.t option
		= fun v1 v2 ->
		let rec _iso optr v1 v2 =
			match v1, v2 with
			| Rtree.Nil, Rtree.Nil -> (optr, true)
			| Rtree.Nil, _ | _, Rtree.Nil -> (None, false)
			| Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2) ->
				match Coeff.cmpz n1, Coeff.cmpz n2 with
				| 0, 0 ->
					let (optr1, res1) = _iso optr l1 l2 in
					if res1 then
						_iso optr1 r1 r2
					else
						(None, false)
				| 0,_ | _,0 -> (None, false)
				| _,_ ->
					let (res, r) =
						let ratio = Coeff.div n1 n2 in
						match optr with
						| None -> (true, Some ratio)
						| Some r -> (Coeff.cmp r ratio = 0, optr)
					in
					if res then
						let (_, res1) = _iso r l1 l2 in
						if res1 then
							_iso r r1 r2
						else
							(None, false)
					else
						(None, false)
		in
		let (ratio, _) = _iso None v1 v2 in ratio

	let nil = Rtree.Nil

	let getVars: t list -> Var.Set.t
		= fun l -> Rtree.mskBuild (fun n -> Coeff.cmpz n <> 0) l |> Rtree.pathsGet

	let rec cmp : t -> t -> int
		= fun v1 v2 ->
		match (v1,v2) with
		| (Rtree.Nil, Rtree.Nil) -> 0
		| (Rtree.Nil, Rtree.Sub (_, _, _)) -> -1
		| (Rtree.Sub (_, _, _), Rtree.Nil) -> 1
		| (Rtree.Sub (l1, n1, r1),Rtree.Sub (l2, n2, r2)) ->
			match Coeff.cmp n1 n2 with
			| 0 -> begin
				match cmp l1 l2 with
				| 0 -> cmp r1 r2
				| r -> r
				end
			| r -> r

	let to_string: (Var.t -> string) -> t -> string
		= fun varPr v ->
			let nodePr a x =
				if Coeff.cmpz a = 0 then
					""
				else
					(Coeff.to_string a) ^ "." ^ x
			in
			let s = Rtree.to_string " + " nodePr varPr v in
			if String.compare s "" = 0
			then "0"
			else s

	let elim (v: Var.t) (using: t) (from: t): t =
		let n1 = get from v in
		match Coeff.cmpz n1 with
		| 0 -> from
		| _ ->
			let n2 = get using v in
			match Coeff.cmpz n2 with
			| 0 -> invalid_arg "Vec.elim"
			| _ -> add (mulc (Coeff.neg n1) using) (mulc n2 from)

	let shift: Var.t -> t -> Var.t option Rtree.t -> Var.t * t * Var.t option Rtree.t
		= fun nxt0 vec0 relocTbl0 ->
		let rec _shift nxt wip ve relocTbl =
			match ve, relocTbl with
			| Rtree.Nil, _ -> (nxt, wip, relocTbl)
			| Rtree.Sub (l, n, r), Rtree.Nil ->
				let (nxt1, wip1, reloc) =
					if Coeff.cmpz n = 0 then
						(nxt, wip, None)
					else
						(Var.next nxt, set wip nxt n, Some nxt)
				in
				let (nxt2, wip2, lReloc) = _shift nxt1 wip1 l Rtree.Nil in
				let (nxt3, wip3, rReloc) = _shift nxt2 wip2 r Rtree.Nil in
				(nxt3, wip3, Rtree.Sub (lReloc, reloc, rReloc))
			| Rtree.Sub (l, n, r), Rtree.Sub (lReloc, reloc, rReloc) ->
				let (nxt1, wip1, reloc1) =
					if Coeff.cmpz n = 0 then
						(nxt, wip, reloc)
					else
						match reloc with
						| Some x -> (nxt, set wip x n, reloc)
						| None -> (Var.next nxt, set wip nxt n, Some nxt)
				in
				let (nxt2, wip2, lReloc1) = _shift nxt1 wip1 l lReloc in
				let (nxt3, wip3, rReloc1) = _shift nxt2 wip2 r rReloc in
				(nxt3, wip3, Rtree.Sub (lReloc1, reloc1, rReloc1))
		in
		_shift nxt0 Rtree.Nil vec0 relocTbl0

	let toRat : t -> Scalar.Rat.t Rtree.t
		= fun v ->
		Rtree.map Coeff.toQ v

	let ofRat : Scalar.Rat.t Rtree.t -> t
		= fun v ->
		Rtree.map Coeff.ofQ v

	let ofSymbolic : Scalar.Symbolic.t -> Coeff.t
		= fun _ ->
		Stdlib.failwith "Vector.ofSymbolic : not implemented"

	let rec dot_productr : Scalar.Rat.t Rtree.t -> t -> Coeff.t
		= fun q v ->
		match (q, v) with
		| (Rtree.Nil, Rtree.Nil)
		| (Rtree.Sub _, Rtree.Nil)
		| (Rtree.Nil, Rtree.Sub _) -> Coeff.z
		| (Rtree.Sub (l1, n1, r1), Rtree.Sub (l2, n2, r2)) ->
			Coeff.add
				(Coeff.mulr n1 n2)
				(Coeff.add (dot_productr l1 l2) (dot_productr r1 r2))

    let project : Var.t list -> t -> t
        = fun vars vec ->
        List.fold_left
            (fun vec var -> set vec var Coeff.z)
            vec vars

    let rename : Var.t -> Var.t -> t -> t
        = fun fromX toY vec ->
		let vec1 = set vec fromX Coeff.z in
		let vec2 = set vec1 toY (get vec fromX) in
		assert (Coeff.cmpz (get vec toY) = 0);
        vec2

    let rename_f : (Var.t -> Var.t) -> t -> t
		= fun f vec ->
        getVars [vec]
        |> Var.Set.elements
        |> List.fast_sort (fun v1 v2 -> Var.cmp v2 v1)
		|> List.fold_left (
            fun vec' var -> rename var (f var) vec'
            ) vec

    let gcd _ = failwith "unimplemented"
end

module Rat = struct
    module Coeff = Scalar.Rat
	include Make (Scalar.Rat)

	let ofSymbolic : Scalar.Symbolic.t -> Scalar.Rat.t
		= fun v ->
		if Scalar.Symbolic.hasDelta v
		then Scalar.Rat.add
				(Scalar.Symbolic.get_v v)
				(Scalar.Rat.mul Scalar.Rat.delta (Scalar.Symbolic.get_d v))
		else Scalar.Symbolic.get_v v


	let gcd v =
		let gcd = Rtree.fold (fun _ g a ->
			if Scalar.Rat.cmpz a = 0 then
				g
			else
				let sofRat = Scalar.Rat.ofQ in
				match g with
				| None -> Some (Scalar.Rat.toZ (Scalar.Rat.abs a |> sofRat))
				| Some g -> Some (Scalar.Rat.gcd g a)) None v
		in
		match gcd with
		| None -> Scalar.Rat.u
		| Some (nGcd, dGcd) -> Scalar.Rat.ofZ dGcd nGcd |> Scalar.Rat.toQ
end

module Float = struct
    module Coeff = Scalar.Float
	include Make(Scalar.Float)

	let ofSymbolic : Scalar.Symbolic.t -> Scalar.Float.t
		= fun v ->
		if Scalar.Symbolic.hasDelta v
		then Scalar.Float.add
				(Scalar.Symbolic.get_v v |> Scalar.Float.ofQ)
				(Scalar.Float.mul Scalar.Float.delta (Scalar.Symbolic.get_d v |> Scalar.Float.ofQ))
		else Scalar.Symbolic.get_v v |> Scalar.Float.ofQ
end


module Symbolic = struct
    module Coeff = Scalar.Symbolic
	include Make(Scalar.Symbolic)

	let ofSymbolic v = v
end

module Int = struct
    module Coeff = Scalar.Int
	include Make(Scalar.Int)

	let ofSymbolic : Scalar.Symbolic.t -> Scalar.Int.t
		= fun _ ->
		Stdlib.failwith "Vector.Int.ofSymbolic"
end
