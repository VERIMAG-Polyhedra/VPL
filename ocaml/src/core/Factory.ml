module Cs = Cstr.Rat

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

let add_mul : 'c t -> 'c -> 'c -> Scalar.Rat.t -> 'c
    = fun factory cert1 cert2 coeff ->
    factory.add cert1 (factory.mul coeff cert2)

let linear_combination : 'c t -> ('c * Scalar.Rat.t) list -> 'c
	= fun factory l ->
	List.fold_left (fun res (cert,n) ->
        add_mul factory res cert n
    ) factory.top l
