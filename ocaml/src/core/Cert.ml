module Cs = Cstr.Rat.Positive

type 'c t =
  { name : string;
	 top : 'c; (* NB: top = triv Cstr_type.Eq Nb.z *)
	 triv: Cstr_type.cmpT -> Scalar.Rat.t -> 'c; (* 0 cmp n*)
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
