module Cs = Cstr.Rat.Positive

type 'c t = (Var.t * 'c Cons.t) list

val to_string: (Var.t -> string) -> 'c t -> string
val to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string

type 'c rel_t =
	| NoIncl
	| Incl of 'c list

val nil : 'c t

val isTop: 'c t -> bool
val list : 'c t -> 'c Cons.t list

(** [filter factory s c] replaces in [c] each variable defined in [s] by its definition. *)
val filter : 'c Factory.t -> 'c t -> 'c Cons.t -> 'c Cons.t

(** [filter2 factory eqset cstr] returns a couple [(cstr',cert)] where
{ul
	{- [cstr'] is [cstr] where variables have been substitued by their definition in [eqset].}
	{- [cert] is the combination of constraints of [eqset] that must be added to [cstr] to obtain [cstr']}
}.
For instance, [filter2 f (x = 2y+1) (2x <= 1)] returns [(4y<=-1, 2x - 4y = 2)].*)
val filter2 : 'c Factory.t -> 'c t -> Cs.t -> Cs.t * 'c Cons.t

val implies : 'c Factory.t -> 'c t -> 'c Cons.t -> bool

val incl : 'c1 Factory.t -> 'c1 t -> 'c2 t -> 'c1 rel_t

val satisfy : 'c t -> Cs.Vec.t -> bool

(** Does not check certificates. *)
val equal: 'c1 t -> 'c2 t -> bool

val choose : Cs.t -> Var.t * Cs.Vec.Coeff.t

val rename: 'c Factory.t -> 'c t -> Var.t -> Var.t -> 'c t

val pick: Var.t option Rtree.t -> 'c Cons.t -> Var.t option

(** [subst factory x c s] substitutes [x] in [s] by its definition in [c]. *)
val subst: 'c Factory.t -> Var.t -> 'c Cons.t -> 'c t -> 'c t

val tryDefs: 'c Factory.t -> Var.t option Rtree.t -> 'c t -> ('c Cons.t * Var.t) option * 'c t

val trySubstM: 'c Factory.t -> Var.t option Rtree.t -> 'c t -> ('c Cons.t * Var.t) option * 'c t

val trySubst: 'c Factory.t -> Var.t -> 'c t -> 'c Cons.t option * 'c t

type 'c meetT =
| Added of 'c t
| Bot of 'c

val meetEq: 'c meetT -> 'c meetT -> bool
val meet_to_string : 'c Factory.t -> (Var.t -> string) -> 'c meetT -> string

val addM: 'c Factory.t -> 'c t -> 'c Cons.t list -> 'c meetT
val add: 'c Factory.t -> 'c t -> 'c Cons.t -> 'c meetT


val joinSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val joinSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val minkowskiSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list

val minkowskiSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (Var.t * (('c1,'c2) Cons.discr_t) Cons.t) list
