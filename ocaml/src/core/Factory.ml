module Cs = Cstr.Rat.Positive

type 'c t = {
    name : string;
    top : 'c;
    triv: Cstr_type.cmpT -> Scalar.Rat.t -> 'c;
    add : 'c -> 'c -> 'c;
    mul : Scalar.Rat.t -> 'c -> 'c;
    merge : 'c -> 'c -> 'c;
    to_le : 'c -> 'c;
    to_string : 'c -> string;
    rename : Var.t -> Var.t -> 'c -> 'c;
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
