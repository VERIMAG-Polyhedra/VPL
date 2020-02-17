module Cs = Cstr.Rat
type 'c t = Cs.t * 'c

let triv : 'c Factory.t -> 'c t
	= fun factory ->
	(Cs.eq [] Scalar.Rat.z, factory.Factory.top)

let mkTriv : 'c Factory.t -> Cstr_type.cmpT -> Scalar.Rat.t -> 'c t
	= fun factory cmp r ->
	(Cs.mk cmp [] r, factory.Factory.triv cmp r)

let get_c (c,_) = c

let get_cert (_,cert) = cert

let to_string : (Var.t -> string) -> 'c t -> string
	= fun varPr (c,_) ->
	Cs.to_string varPr c

let to_string_ext : 'c Factory.t -> (Var.t -> string) -> 'c t -> string
	= fun factory varPr (c,cert) ->
	Printf.sprintf "%s: %s" (Cs.to_string varPr c) (factory.Factory.to_string cert)

let equal (c1,_) (c2,_) = Cs.equal c1 c2

let implies (c1,_) (c2,_) = Cs.incl c1 c2

let elim : 'c Factory.t -> Var.t -> 'c t -> Cs.t -> Cs.t * 'c t
	= fun factory x (eq,eq_cert) cstr ->
	if Cs.Vec.Coeff.cmpz (Cs.Vec.get (Cs.get_v cstr) x) = 0
	then (cstr, (Cs.eq [] Scalar.Rat.z, factory.Factory.top))
	else
		let (c, n1, n2) = Cs.elim eq cstr x in
		let coeff = Scalar.Rat.div n1 n2
			|> Scalar.Rat.neg in
		let cstr = Cs.mulc coeff eq in
		let cert = factory.Factory.mul coeff eq_cert in
		(c, (cstr,cert))

let rename : 'c Factory.t -> Var.t -> Var.t -> 'c t -> 'c t
	= fun factory fromX toY (c,cert) ->
	let c' = Cs.rename fromX toY c in
	let cert' = factory.Factory.rename fromX toY cert in
	(c',cert')

let linear_combination_cons : 'c Factory. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c t
	= fun factory conss witness ->
	List.fold_left
		(fun (cstr_res, cert_res) (i,n) ->
			let (cstr,cert) = List.nth conss i in
			Cs.add
				cstr_res
				(Cs.mulc n cstr)
			,
			factory.Factory.add
				cert_res
				(factory.Factory.mul n cert))
		(triv factory)
		witness

let linear_combination_cert : 'c Factory. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c
	= fun factory conss witness ->
	try
		List.fold_left
			(fun res (i,n) ->
				let cert = List.nth conss i
					|> get_cert
				in
				factory.Factory.add
					res
					(factory.Factory.mul n cert))
			factory.Factory.top
			witness
  with _ -> Stdlib.invalid_arg "Cons.linear_combination"

let add : 'c Factory.t -> 'c t -> 'c t -> 'c t
	= fun factory (c1,cert1) (c2,cert2)->
	(Cs.add c1 c2, factory.Factory.add cert1 cert2)

let mul : 'c Factory.t -> Scalar.Rat.t -> 'c t -> 'c t
	= fun factory r (c,cert) ->
	(Cs.mulc r c, factory.Factory.mul r cert)

let split : 'c Factory.t -> 'c t -> 'c t * 'c t
	= fun factory eq ->
	match get_c eq |> Cs.get_typ with
	| Cstr_type.Le | Cstr_type.Lt -> Stdlib.invalid_arg "Cons.split"
	| Cstr_type.Eq ->
		let triv = mkTriv factory Cstr_type.Le Scalar.Rat.z in
		let up = add factory eq triv in
		let low = add factory (mul factory Scalar.Rat.negU eq) triv in
		(up,low)

let normalize : 'c Factory.t -> 'c t -> 'c t
	= fun factory (cstr, cert) ->
	let gcd = Cs.Vec.gcd (Cs.get_v cstr) in
	mul factory gcd (cstr, cert)

let elimc : 'c Factory.t -> Var.t -> 'c t -> 'c t -> 'c t
	= fun factory x (c1,cert1) (c2,cert2) ->
		let (c, n1, n2) = Cs.elim c1 c2 x in
		(c, Factory.linear_combination factory [cert1, n1; cert2, n2])
		|> normalize factory

type ('c1,'c2) discr_t = 'c1 * 'c2

type ('c1,'c2) discr_cert = (('c1,'c2) discr_t) Factory.t

let discr_factory : 'c1 Factory.t -> 'c2 Factory.t -> ('c1,'c2) discr_cert
	= fun fac1 fac2 ->
	Factory.({
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

let joinSetup_1 : 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t
	= fun factory2 nxt relocTbl alpha (c,cert) ->
	let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
	let (vec2, alphaCoef, cst) = (vec1, Cs.Vec.Coeff.neg (Cs.get_c c), Cs.Vec.Coeff.z)
	in
	let c' = {c with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
	let cert' = (cert, factory2.Factory.top) in
	(nxt1, relocTbl1, (c',cert'))

let joinSetup_2 : 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t
	= fun factory1 nxt relocTbl alpha (c,cert) ->
	let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
	let (vec2, alphaCoef, cst) = (Cs.Vec.add (Cs.get_v c) (Cs.Vec.neg vec1), Cs.get_c c, Cs.get_c c)
	in
	let c' = {c with Cs.v = Cs.Vec.set vec2 alpha alphaCoef; Cs.c = cst} in
	let cert' = (factory1.Factory.top, cert) in
	(nxt1, relocTbl1, (c',cert'))

let minkowskiSetup_1 : 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t
	= fun factory2 nxt relocTbl (c,cert) ->
	let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
	let c' = {c with Cs.v = vec1} in
	let cert' = (cert, factory2.Factory.top) in
	(nxt1, relocTbl1, (c',cert'))

let minkowskiSetup_2 : 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t
	= fun factory1 nxt relocTbl (c,cert) ->
	let (nxt1, vec1, relocTbl1) = Cs.Vec.shift nxt (Cs.get_v c) relocTbl in
	let vec2 = Cs.Vec.add (Cs.get_v c) (Cs.Vec.neg vec1) in
	let c' = {c with Cs.v = vec2} in
	let cert' = (factory1.Factory.top, cert) in
	(nxt1, relocTbl1, (c',cert'))

let rec clean : 'c t list -> 'c t list
    = function
    | [] -> []
    | (c,cert) :: l ->
        match Cs.tellProp c with
        | Cs.Contrad -> invalid_arg "clean: contradictory constraint"
        | Cs.Trivial -> clean l
        | Cs.Nothing ->
            if List.exists (fun (c',_) -> Cs.equal c c') l
            then clean l
            else (c,cert) :: clean l

let adjust_cert_constant : 'c Factory.t -> 'c t -> Cs.t -> 'c
	= fun factory (c1,cert1) c2 ->
	let v1 = Cs.get_v c1 in
	let v2 = Cs.get_v c2 in
	match Vector.Rat.isomorph v2 v1 with
	| Some r -> (* v2 = r.v1 *)
		begin
		let cert =
			let cste = Scalar.Rat.sub
				(Cs.get_c c2)
				(Scalar.Rat.mul r (Cs.get_c c1))
			in
            if Cs.get_typ c2 = Cstr_type.Lt && Scalar.Rat.equal r Scalar.Rat.u && Scalar.Rat.equal cste Scalar.Rat.z
            then cert1
			else if Scalar.Rat.lt cste Scalar.Rat.z
    			then factory.Factory.mul r cert1
    			else
                    let cste_cert = factory.Factory.triv (Cs.get_typ c2) cste
    				in
    				factory.Factory.mul r cert1
    				|> factory.Factory.add cste_cert
    	in
		match Cs.get_typ c1, Cs.get_typ c2 with
		| Cstr_type.Lt, Cstr_type.Le -> factory.Factory.to_le cert
		| _,_ -> cert
		end
	| None -> Stdlib.invalid_arg "Inclusion does not hold"
