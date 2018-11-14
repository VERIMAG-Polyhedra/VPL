module Cs = Cstr.Rat.Positive

type 'c t = Cs.t * 'c

val triv : 'c Cert.t -> 'c t
val mkTriv : 'c Cert.t -> Cstr_type.cmpT -> Scalar.Rat.t -> 'c t

val get_c : 'c t -> Cs.t
val get_cert : 'c t -> 'c
val to_string : (Cs.Vec.V.t -> string) -> 'c t -> string
val to_string_ext : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string

val equal : 'c t -> 'c t -> bool

(** [implies c1 c2] returns [true] if [c1] implies [c2]. *)
val implies: 'c t -> 'c t -> bool
val elimc : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c t -> 'c t
val elim : 'c Cert.t -> Cs.Vec.V.t -> 'c t -> Cs.t -> Cs.t * 'c t
val rename : 'c Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t -> 'c t

val linear_combination_cert : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c
val linear_combination_cons : 'c Cert. t -> 'c t list -> (int * Scalar.Rat.t) list -> 'c t

val add : 'c Cert.t -> 'c t -> 'c t -> 'c t
val mul : 'c Cert.t -> Scalar.Rat.t -> 'c t -> 'c t
val split : 'c Cert.t -> 'c t -> 'c t * 'c t

val normalize : 'c Cert.t -> 'c t -> 'c t

type ('c1,'c2) discr_t = 'c1 * 'c2

type ('c1,'c2) discr_cert = (('c1,'c2) discr_t) Cert.t

val discr_factory : 'c1 Cert.t -> 'c2 Cert.t -> ('c1,'c2) discr_cert

val joinSetup_1 : 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Rtree.t -> Cs.Vec.V.t -> 'c1 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Rtree.t * (('c1,'c2) discr_t) t

val joinSetup_2 : 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Rtree.t -> Cs.Vec.V.t -> 'c2 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Rtree.t * (('c1,'c2) discr_t) t

val minkowskiSetup_1 : 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Rtree.t -> 'c1 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Rtree.t * (('c1,'c2) discr_t) t

val minkowskiSetup_2 : 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Rtree.t -> 'c2 t
    	-> Cs.Vec.V.t * Cs.Vec.V.t option Rtree.t * (('c1,'c2) discr_t) t
