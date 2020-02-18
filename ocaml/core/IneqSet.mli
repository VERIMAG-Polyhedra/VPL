(** Data structure that hosts the inequalities of a polyhedron.
    It represents only sets of inequalities with {b nonempty interior}, meaning that they must be feasible and they should not contain any implicit equality. *)

module Debug : DebugTypes.Type

(** Type of inequality set. *)
type 'c t = {
    ineqs : 'c Cons.t list; (** Inequalities of the polyhedron *)
    regions : 'c PLP.Region.t list option; (** Regions of a polyhedron *)
}

val top : 'c t

(** @return true if the polyhedron is top. *)
val is_top : 'c t -> bool

val of_list : 'c Cons.t list -> 'c t

(** Conversion into string. *)
val to_string_raw : 'c t -> string

(** Conversion into string.-
    @param varPr the variable printer *)
val to_string: (Var.t -> string) -> 'c t -> string

(** Conversion into string.
    Also prints the certificates.
    @param factory the factory
    @param varPr the variable printer *)
val to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string

(** Applies a function to each certificate.
    @param f the funciton to apply *)
val map : ('c1 -> 'c2) -> 'c1 t -> 'c2 t

(** Result of a syntactic inclusion test.
    Certificates are the constraints that must be added to obtain the associated result.*)
type 'c prop_t =
| Empty of 'c
| Trivial
| Implied of 'c
| Check of 'c Cons.t

(** Checks if a constraint includes an inequality set.
    The constraint is first rewritten with an equality set.
    @param factory the factory
    @param es an equality set
    @param s an inequality set
    @param cstr a constraint *)
val synIncl : 'c Factory.t -> 'c EqSet.t -> 'c t -> Cstr.Rat.t -> 'c prop_t

(** Result of an inclusion test. *)
type 'c rel_t =
| NoIncl
| Incl of 'c list

(** Checks the inclusion between two inequality sets.
    The constraint is first rewritten with an equality set.
    @param factory the factory
    @param the horizon variable
    @param es an equality set
    @param s1 an inequality set
    @param s2 an inequality set
    Tests if [s2] includes [s1]. *)
val incl: 'c1 Factory.t -> Var.t -> 'c1 EqSet.t -> 'c1 t ->  'c2 t -> 'c1 rel_t

(** Variable renaming in an inequality set
    @param factory the factory
    @param s an inequality set
    @param fromX variable to rename
    @param toY new variable name *)
val rename : 'c Factory.t -> 'c t -> Var.t -> Var.t -> 'c t

(** Assume a list of constraints.
    @param h horizon variable
    @param s the initial inequality set
    @param conss the list of constraints to assume
    @param point a point within the polyhedron's interior *)
val assume: Var.t -> 'c t -> 'c Cons.t list -> Scalar.Symbolic.t Rtree.t -> 'c t

val assume_back : 'c Factory.t -> 'c t -> 'c Cons.t -> Vector.Symbolic.t -> ('c t * Vector.Symbolic.t) option

(** Substitutes a variable with an expression in an equality set.
    @param factory the factory
    @param h the hoizon variable
    @param es the equality set
    @param x variable to substiture
    @param e the equality that defines [x]
    @param s the inequality set to rewrite*)
val subst: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t -> 'c Cons.t -> 'c t -> 'c t

(** Eliminates a variable with Fourier-Motzkin elimination.
    @param factory the factory
    @param h the horizon variable
    @param es the equality set
    @param mask variables to eliminate
    @param s the inequality set *)
val fmElim: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t option Rtree.t -> 'c t -> 'c t

(** Eliminates a a variable by PLP.
    @param factory the factory
    @param point the normalization point
    @param vars variables to eliminate
    @param s the inequality set *)
val plpElim : 'c Factory.t -> Vector.Rat.t -> Var.t list -> 'c t -> 'c t

val proj_incl : 'c Factory.t -> Vector.Rat.t -> Var.t list -> 'c EqSet.t -> 'c t -> 'c t -> 'c t option

val get_regions_from_point : 'c Factory.t -> 'c t -> Vector.Rat.t -> ('c Cons.t list * Vector.Symbolic.t) list

(** @return true if the given point satisfies the constraints. *)
val satisfy : 'c t -> Vector.Rat.t -> bool

(**/**)
val joinSetup_1: 'c2 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c1 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
val joinSetup_2: 'c1 Factory.t -> Var.t -> Var.t option Rtree.t -> Var.t -> 'c2 t
	-> Var.t * Var.t option Rtree.t * (('c1,'c2) Cons.discr_t) Cons.t list
val pick : Var.t option Rtree.t -> 'c t -> Var.t option
val fmElim_one: 'c Factory.t -> Var.t -> 'c EqSet.t -> Var.t ->  'c t -> 'c t
(**/**)
