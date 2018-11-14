module Cs = Cstr.Rat.Positive

type 'c t = (Cs.Vec.V.t * 'c Cons.t) list

val to_string: (Cs.Vec.V.t -> string) -> 'c t -> string
val to_string_ext: 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c t -> string

type 'c rel_t =
	| NoIncl
	| Incl of 'c list

val nil : 'c t

val isTop: 'c t -> bool
val list : 'c t -> 'c Cons.t list

(** [filter factory s c] replaces in [c] each variable defined in [s] by its definition. *)
val filter : 'c Cert.t -> 'c t -> 'c Cons.t -> 'c Cons.t

(** [filter2 factory eqset cstr] returns a couple [(cstr',cert)] where
{ul
	{- [cstr'] is [cstr] where variables have been substitued by their definition in [eqset].}
	{- [cert] is the combination of constraints of [eqset] that must be added to [cstr] to obtain [cstr']}
}.
For instance, [filter2 f (x = 2y+1) (2x <= 1)] returns [(4y<=-1, 2x - 4y = 2)].*)
val filter2 : 'c Cert.t -> 'c t -> Cs.t -> Cs.t * 'c Cons.t

val implies : 'c Cert.t -> 'c t -> 'c Cons.t -> bool

val incl : 'c1 Cert.t -> 'c1 t -> 'c2 t -> 'c1 rel_t

val satisfy : 'c t -> Cs.Vec.t -> bool

(** Does not check certificates. *)
val equal: 'c1 t -> 'c2 t -> bool

val choose : Cs.t -> Cs.Vec.V.t * Cs.Vec.Coeff.t

val rename: 'c Cert.t -> 'c t -> Cs.Vec.V.t -> Cs.Vec.V.t -> 'c t

val pick: Cs.Vec.V.t option Rtree.t -> 'c Cons.t -> Cs.Vec.V.t option

(** [subst factory x c s] substitutes [x] in [s] by its definition in [c]. *)
val subst: 'c Cert.t -> Cs.Vec.V.t -> 'c Cons.t -> 'c t -> 'c t

val tryDefs: 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t

val trySubstM: 'c Cert.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c t -> ('c Cons.t * Cs.Vec.V.t) option * 'c t

val trySubst: 'c Cert.t -> Cs.Vec.V.t -> 'c t -> 'c Cons.t option * 'c t

type 'c meetT =
| Added of 'c t
| Bot of 'c

val meetEq: 'c meetT -> 'c meetT -> bool
val meet_to_string : 'c Cert.t -> (Cs.Vec.V.t -> string) -> 'c meetT -> string

val addM: 'c Cert.t -> 'c t -> 'c Cons.t list -> 'c meetT
val add: 'c Cert.t -> 'c t -> 'c Cons.t -> 'c meetT


val joinSetup_1: 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c1 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val joinSetup_2: 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> Cs.Vec.V.t -> 'c2 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val minkowskiSetup_1: 'c2 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c1 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val minkowskiSetup_2: 'c1 Cert.t -> Cs.Vec.V.t -> Cs.Vec.V.t option Cs.Vec.M.t -> 'c2 t
	-> Cs.Vec.V.t * Cs.Vec.V.t option Cs.Vec.M.t * (Cs.Vec.V.t * (('c1,'c2) Cons.discr_t) Cons.t) list
