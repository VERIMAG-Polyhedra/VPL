open Cstr_type

module type Type = Type

let cmpT_extended_to_string = function
	| EQ -> "="
	| LE  -> "<="
	| LT  -> "<"
	| GE -> ">="
	| GT -> ">"
	| NEQ -> "≠"

let cmpT_to_string = function
  | Eq -> "="
  | Le -> "<="
  | Lt -> "<";;

module Make (Vec : Vector.Type) = struct

	module Coeff = Vec.Coeff

	let name = "Constraint with vector type : " ^ (Vec.name)

	type t = {
		typ: cmpT;
		v: Vec.t;
		c: Coeff.t }

	let get_typ (x : t) = x.typ
	let get_v (x : t) = x.v
	let get_c (x : t) = x.c

	type prop_t =
	| Trivial
	| Contrad
	| Nothing

	let cmpAdd : cmpT -> cmpT -> cmpT
	   = fun o1 o2 ->
		match o1 with
		| Eq -> o2
		| Lt as o -> o
		| Le as o ->
			match o2 with
			| Eq | Le -> o
			| Lt as o -> o

	let eval : t -> Vec.t -> Coeff.t
		= fun c pt ->
		Coeff.sub (Vec.eval c.v pt) c.c

	let add : t -> t -> t
		= fun c1 c2 ->
		{typ = cmpAdd c1.typ c2.typ;
		v = Vec.add c1.v c2.v;
		c = Coeff.add c1.c c2.c }

	let mulc : Coeff.t -> t -> t
		= fun c cstr ->
		if cstr.typ <> Eq && Coeff.le c Coeff.z
		then Stdlib.raise BadMult
		else { cstr with v = Vec.mulc c cstr.v; c = Coeff.mul c cstr.c }


	let mulc_no_exc : Coeff.t -> t -> t
		= fun c cstr -> { cstr with
            v = Vec.mulc c cstr.v;
            c = Coeff.mul c cstr.c
        }

	let compl : t -> t
		= fun c ->
		match c.typ with
		| Eq -> invalid_arg "Cstr.compl"
		| Le -> { typ = Lt; v = Vec.neg c.v; c = Coeff.neg c.c }
		| Lt -> { typ = Le; v = Vec.neg c.v; c = Coeff.neg c.c }

	let split : t -> t * t
		= fun c ->
		match c.typ with
		| Le | Lt -> invalid_arg "Cstr.split"
		| Eq -> ({ typ = Le; v = c.v; c = c.c },
			{ typ = Le; v = Vec.neg c.v; c = Coeff.neg c.c })

	let mk typ v c = { typ = typ; v = Vec.mk v; c = c }
	let mk2 typ v c = { typ = typ; v = v; c = c }
	let eq v c = mk Eq v c
	let le v c = mk Le v c
	let lt v c = mk Lt v c

    let top = eq [] Vec.Coeff.z

	let mkInt typ v c =
		match typ with
		| Eq | Le -> { typ = typ; v = Vec.mk v; c = c }
		| Lt -> { typ = Le; v = Vec.mk v; c = Coeff.sub c Coeff.u }

	let equalSyn c1 c2 =
		c1.typ = c2.typ && Vec.equal c1.v c2.v && Coeff.equal c1.c c2.c

	let inclSyn c1 c2 =
		if Vec.equal c1.v c2.v then
			match c1.typ, c2.typ with
			| Eq, Eq -> Coeff.cmp c1.c c2.c = 0
			| Eq, Le | Le, Le
			| Lt, Lt | Lt, Le -> Coeff.cmp c1.c c2.c <= 0
			| Eq, Lt | Le, Lt -> Coeff.cmp c1.c c2.c < 0
			| Le, Eq | Lt, Eq -> false
		else
			false

	let equal c1 c2 =
		if c1.typ <> c2.typ then
			false
		else

		match Vec.isomorph c1.v c2.v with
		| None ->
			if Vec.equal Vec.nil c1.v && Vec.equal Vec.nil c2.v then
				Coeff.cmp c1.c c2.c = 0
			else
				false

		| Some ratio ->
			if Coeff.cmpz ratio > 0 then
				if c1.typ = Eq then
					Coeff.cmp c1.c (Coeff.mul ratio c2.c) = 0
				else
					false
			else
				Coeff.cmp c1.c (Coeff.mul ratio c2.c) = 0

	let incl c1 c2 =
		if Vec.equal Vec.nil c2.v
		then let r = Coeff.cmpz c2.c in
			(match c2.typ with
			| Eq -> r = 0
			| Le -> r <= 0
			| Lt -> r < 0)
		else if Vec.equal Vec.nil c1.v
			then false
			else match Vec.isomorph c1.v c2.v with
				| None -> false
				| Some ratio ->
					let res = Coeff.cmp c1.c (Coeff.mul ratio c2.c) in
					match c1.typ, c2.typ with
					| Eq, Eq -> res = 0
					| Le, Le | Lt, Lt
					| Lt, Le -> Coeff.cmpz ratio < 0 && res <= 0
					| Le, Lt -> Coeff.cmpz ratio < 0 && res < 0
					| Eq, Le -> res <= 0
					| Eq, Lt -> res < 0
					| Le, Eq | Lt, Eq -> false

	let tellProp c =
		if Vec.equal Vec.nil c.v then
			let r = Coeff.cmpz c.c in
			match c.typ with
			| Eq -> if r = 0 then Trivial else Contrad
			| Le -> if r <= 0 then Trivial else Contrad
			| Lt -> if r < 0 then Trivial else Contrad
		else
			Nothing

	let getVars: t list -> Var.Set.t
	= fun l -> List.map get_v l |> Vec.getVars

	let getCoefsFor : Var.t option -> t list -> Coeff.t list
	= function
		| None -> fun l -> List.map get_c l
		| Some x -> fun l -> List.map (fun c -> Vec.get (get_v c) x) l

	let to_string : (Var.t -> string) -> t -> string
		= fun varPr c ->
			let sign =
				match c.typ with
				| Eq -> " = "
				| Le -> " <= "
				| Lt -> " < "
			in
			(Vec.to_string varPr c.v) ^ sign ^ (Coeff.to_string c.c)

	let plot : Var.t list -> t -> string
		= fun vars c ->
		let vec = get_v c in
		let l = (get_c c) ::
			(List.map
				(fun v -> Coeff.mul Coeff.negU (Vec.get vec v))
				vars)
		in
		Misc.list_to_string Coeff.to_string l ","

	let list_plot : t list -> string
		= fun cstrs ->
		let vars = getVars cstrs |> Var.Set.elements in
		Misc.list_to_string (plot vars) cstrs ", "

	let list_to_string : t list -> string
		= fun l ->
		Misc.list_to_string (to_string Var.to_string) l " ; "


	let cmp : t -> t -> int
		= fun c1 c2 ->
		match (c1.typ,c2.typ) with
		| (Eq,Le) | (Le,Lt) | (Eq,Lt) -> -1
		| (Le,Eq) | (Lt,Le) | (Lt,Eq) -> 1
		| (_,_) ->	begin
			match Vec.cmp c1.v c2.v with
			| 0 -> Coeff.cmp c1.c c2.c
			| r -> r
			end

	let satisfy : Vec.t -> t -> bool
		= fun point c ->
		let res = Rtree.fold (fun _ -> Coeff.add) Coeff.z (Vec.mul_t point c.v) in
		let r = Coeff.cmp res c.c in
		match c.typ with
		| Eq -> r = 0
		| Le -> r <= 0
		| Lt -> r < 0

	let saturate : Vec.t -> t -> bool
		= fun point c ->
		satisfy point {c with typ = Eq}

	let rename : Var.t -> Var.t -> t -> t
		= fun fromX toY c ->
		{c with v = Vec.rename fromX toY (get_v c)}

	let rename_f : (Var.t -> Var.t) -> t -> t
		= fun f cstr ->
        {cstr with v = Vec.rename_f f (get_v cstr)}

	(* TODO: vérifier la présence de x? *)
	let change_variable : Var.t -> Vec.t -> Coeff.t -> t -> t
		= fun x lin c cstr ->
		let v = get_v cstr in
		let coeff = Vec.get v x in
		let v1 = Vec.set v x Vec.Coeff.z in
		let v2 = Vec.mulc coeff lin
			|> Vec.add v1 in
		let c2 = Coeff.sub (get_c cstr) (Coeff.mul coeff c) in
		{cstr with v = v2 ; c = c2}

	let get_saturating_point : t -> Vec.t
		= fun cstr ->
		let (var,coeff) = Vec.toList cstr.v
			|> List.hd in
		Vec.mk [Coeff.div cstr.c coeff, var]

    let distance_point_cstr : Vec.t -> t -> Vec.Coeff.t
        = fun point cstr ->
        let a = get_v cstr in
        let num = Vec.Coeff.sub
            (get_c cstr)
            (Vec.dot_product a point)
        in
        let den = Vec.dot_product a a in
        Vec.Coeff.div num den
end

module Rat = struct
	module Vec = Vector.Rat
	include Make(Vec)

	let elim c1 c2 va =
		let a1 = Vec.get c1.v va in
			  let a2 = Vec.get c2.v va in
		let r1 = Vec.Coeff.cmpz a1 in
			  let r2 = Vec.Coeff.cmpz a2 in
		match (r1 = 0, r2 = 0) with
		| (true, true) -> raise NoElim
		| (true, false) | (false, true) -> raise CannotElim
		| (false, false) ->
			let (n1, n2) =
				match (c1.typ, c2.typ) with
				| (Eq, Eq) -> (Vec.Coeff.neg a2, a1)
				| (Le, Eq) | (Lt, Eq) ->
					if r2 > 0 then (Vec.Coeff.neg a2, a1) else (a2, Vec.Coeff.neg a1)
				| (Eq, Le) | (Eq, Lt) ->
					if r1 > 0 then (a2, Vec.Coeff.neg a1) else (Vec.Coeff.neg a2, a1)
				| Le, Le | Lt, Lt
				| Lt, Le | Le, Lt ->
					match (r1 > 0, r2 > 0) with
					| (true, true) | (false, false) -> raise CannotElim
					| (true, false) -> (a2, Vec.Coeff.neg a1)
					| (false, true) -> (Vec.Coeff.neg a2, a1)
			in
			let c = add (mulc n1 c1) (mulc n2 c2) in
			(c, n1, n2)

	let canon : t -> t
		= fun cstr ->
		let g = Vec.gcd cstr.v in
		if Vec.Coeff.isZ g
		then cstr
		else {cstr with v = Vec.mulr g cstr.v ; c = Vec.Coeff.mulr g cstr.c}
end
