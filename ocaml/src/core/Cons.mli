module Cs = Cstr.Rat.Positive

type 'c t = Cs.t * 'c

val triv : 'c Factory.t -> 'c t
val mkTriv : 'c Factory.t -> Cstr_type.cmpT -> Scalar.Rat.t -> 'c t

val get_c : 'c t -> Cs.t
val get_cert : 'c t -> 'c
val to_string : (Var.t -> string) -> 'c t -> string
val to_string_ext : 'c Factory.t -> (Var.t -> string) -> 'c t -> string

val equal : 'c t -> 'c t -> bool

(** [implies c1 c2] returns [true] if [c1] implies [c2]. *)
val implies: 'c t -> 'c t -> bool
val elimc : 'c Factory.t -> Var.t -> 'c t -> 'c t -> 'c t
val elim : 'c Factory.t -> Var.t -> 'c t -> Cs.t -> Cs.t * 'c t
val rename : 'c Factory.t -> Var.t -> Var.t -> 'c t -> 'c t

val linear_combination_cert : 'c Factory.t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c
val linear_combination_cons : 'c Factory.t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c t

val add : 'c Factory.t -> 'c t -> 'c t -> 'c t
val mul : 'c Factory.t -> Scalar.Rat.t -> 'c t -> 'c t
val split : 'c Factory.t -> 'c t -> 'c t * 'c t

val normalize : 'c Factory.t -> 'c t -> 'c t

type ('c1,'c2) discr_t = 'c1 * 'c2

type ('c1,'c2) discr_cert = (('c1,'c2) discr_t) Factory.t

val discr_factory : 'c1 Factory.t -> 'c2 Factory.t -> ('c1,'c2) discr_cert

val joinSetup_1 : 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t

val joinSetup_2 : 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t

val minkowskiSetup_1 : 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t

val minkowskiSetup_2 : 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c2 t
    	-> Var.t * Var.t option Rtree.t * (('c1,'c2) discr_t) t
