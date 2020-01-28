module Cs = Cstr.Rat
module Vec = Cs.Vec
module CP = CstrPoly
module Polynomial = CP.Poly
module Coeff = Scalar.Rat

module type Type = sig

    (** Type of certificates. *)
	type cert

    (** The factory itself. *)
	val factory : cert Factory.t

    (** Builds a certificate from a contraint.*)
	val mk : Cs.t -> cert

    (** Checks if a constraint and a certificate represent the same space. *)
	val equal : Pol.Cs.t -> cert -> bool
end

module Make(F : Type) = struct

    include F

    let mkCons : Cstr.Rat.t -> cert Cons.t
		= fun cs -> (cs, mk cs)

    let convert : 'c Pol.t -> cert Pol.t
        = fun p -> {
            Pol.eqs = List.map (fun (v,(cstr,_)) ->
                (v,mkCons cstr)
            ) p.Pol.eqs;
            Pol.ineqs = List.map (fun (cstr,_) ->
                mkCons cstr
            ) p.Pol.ineqs.ineqs
            |> IneqSet.of_list;
            Pol.point = p.Pol.point;
        }

    let check : cert Pol.t -> bool
		= fun p ->
		List.for_all (fun (c,cert) ->
            equal c cert
        ) (Pol.get_cons p)

    let to_string = factory.to_string
end

module Cstr = struct

	type cert = Pol.Cs.t

	let factory = {
		Factory.name = "Cstr";
		Factory.top = (Cs.mk Cstr_type.Eq [] Scalar.Rat.z);
		Factory.triv = (fun cmp n -> if
			match cmp with
			| Cstr_type.Le -> Scalar.Rat.le Scalar.Rat.z n
			| Cstr_type.Lt -> Scalar.Rat.lt Scalar.Rat.z n
			| Cstr_type.Eq -> Scalar.Rat.equal n Scalar.Rat.z
			then ()
			else (Printf.sprintf "triv %s %s 0"
			 	(Cstr_type.(match cmp with | Le -> "<=" | Lt -> "<" | Eq -> "="))
				(Scalar.Rat.to_string n)
				|> print_endline;
				Stdlib.failwith "Factory.Cstr.triv")
			;
			Cs.mk cmp [] n);
		Factory.add = Cs.add;
		Factory.mul = Cs.mulc;
		Factory.to_le = (fun c -> {c with Cs.typ = Cstr_type.Le});
		Factory.merge = (fun c1 c2 ->
			let c1' = {c1 with Cs.typ = Cstr_type.Eq}
			and c2' = {c2 with Cs.typ = Cstr_type.Eq} in
			if Cs.equal c1' c2'
			then c1'
			else failwith "merge");
		Factory.to_string = Cs.to_string Var.to_string;
		Factory.rename = Cs.rename;
	}

    let mk : Pol.Cs.t -> cert
        = fun cs -> cs

	let equal : Pol.Cs.t -> cert -> bool
		= fun cs cert ->
		Cs.equalSyn cs cert
end

module Unit = struct

	type cert = unit

	let factory = {
		Factory.name = "Unit";
		Factory.top = ();
		Factory.triv = (fun _ _ -> ());
		Factory.add = (fun _ _ -> ());
		Factory.mul = (fun _ _ -> ());
		Factory.to_le = (fun _ -> ());
		Factory.merge = (fun _ _ -> ());
		Factory.to_string = (fun _ -> "unit");
		Factory.rename = (fun _ _ _ -> ());
	}

    let mk : Pol.Cs.t -> cert
        = fun _ -> ()

	let equal : Pol.Cs.t -> cert -> bool
		= fun _ _ -> true
end
