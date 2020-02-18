(** This module handles equality sets.
    Equality sets are maintained in echelon form. *)

module Cs = Cstr.Rat

(** Type of equality set.
    Each equality defines a variable in terms of the others. *)
type 'c t = (Var.t * 'c Cons.t) list

val to_string: (Var.t -> string) -> 'c t -> string
val to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string

(** Empty equality set: top. *)
val nil : 'c t

(** @return true if the given equality set is top. *)
val isTop: 'c t -> bool

(** @return the list of equalities of the set. *)
val list : 'c t -> 'c Cons.t list

(** Rewrites a constraint according to an equality set.
    @param factory the factory
    @param set the equality set
    @param cons the constraint to rewrite
    @return [cons] where each variable in [set] has been substitued with its definition. *)
val filter : 'c Factory.t -> 'c t -> 'c Cons.t -> 'c Cons.t

(** Rewrites a constraint according to an equality set.
    @param factory the factory
    @param set the equality set
    @param cons the constraint to rewrite
    @return [cstr'] which is equal to [cons] where variables have been substitued by their definition in [eqset]
	@return [cert]: the combination of constraints of [set] that must be added to [cons] to obtain [cstr']
    For instance, [filter2 f (x = 2y+1) (2x <= 1)] returns [(4y<=-1, 2x - 4y = 2)].*)
val filter2 : 'c Factory.t -> 'c t -> Cs.t -> Cs.t * 'c Cons.t

(** Result of an inclusion testing. *)
type 'c rel_t =
	| NoIncl
	| Incl of 'c list

(** Tests the inclusion between two equality sets
    @param factory the factory for [set1]
    @param set1 the first equality set
    @param set2 the second equality set
    @return true if [set1] <= [set2], ie. if [set2] includes [set1]. *)
val leq : 'c1 Factory.t -> 'c1 t -> 'c2 t -> 'c1 rel_t

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
