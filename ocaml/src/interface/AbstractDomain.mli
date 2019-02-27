(** Type of abstract domains. *)

(** Polyhedra domain *)
module type Type = sig

	(** Type of polyhedron. *)
	type t

    (** Type of variables. *)
    type var

    (** Type of certificates. *)
    type cert

    (** Type of arithmetic expressions. *)
    type a_expr

    (** Type of boolean expressions. *)
    type b_expr

    val var_to_string : var -> string

    val a_expr_to_string : a_expr -> string

    val b_expr_to_string : b_expr -> string

	(** Unbounded polyhedron. *)
	val top: t

	(** Empty polyhedron. *)
	val bottom: t

	(** Checks if the given polyhedron is empty. *)
	val is_bottom: t -> bool

    (** If the polyhedron is bottom, returns the associated certificate.
        Otherwise, returns None. *)
    val get_bottom_cert : t -> cert option

    val get_cstrs : t -> Cstr.Rat.t list

    val get_b_expr : t -> b_expr

    val get_vars : t -> var list

	(** Computes the intersection of two polyhedra. *)
	val meet : t -> t -> t

	(** Computes the minkowski sum of two polyhedra. *)
	val minkowski: t -> t -> t

	(** Computes the convex hull of two polyhedra. *)
	val join: t -> t -> t

	(** Eliminates the given list of variables from a polyhedron, by orthogonal projection.
        This operator relies on a certified single-variable projection.
        Variables will be eliminated one after the other.
        For a projection that eliminates several variables at the same time, see {!val:project_vars}. *)
	val project: var list -> t -> t

    (** Eliminates the given list of variables from a polyhedron, by orthogonal projection.
        Contrary to {!val:project}, this operator actually eliminates all variables at the same time. *)
	val project_vars: var list -> t -> t

    (** @param p1 the first polyhedron
        @param p2 the second polyhedron
        @return the projection of [p1] on [p2]'s variables
        @return None if the projection of [p1] on [p2]'s variables is not includes in [p2] *)
    val proj_incl : t -> t -> t option

    (** Widening operator. *)
	val widen: t -> t -> t

	(** Test the inclusion of two polyhedra. *)
	val leq: t -> t -> bool

	(** Returns a string representing the given polyhedron.
		Requires a function to convert variables into strings. *)
	val to_string: (var -> string) -> t -> string

	(** Returns the upper bound of an expression in a polyhedron. *)
	val get_upper_bound : a_expr -> t -> Pol.bndT option

	(** Returns the lower bound of an expression in a polyhedron. *)
	val get_lower_bound : a_expr -> t -> Pol.bndT option

	(** Returns both the upper and lower bounds of an expression in a polyhedron. *)
	val itvize : a_expr -> t -> Pol.itvT

	(** Guard operator.
        It computes the intersection between a condition and a polyhedron. *)
	val assume: b_expr -> t -> t

	(** Returns true if the given condition is satisfied in the given polyhedron. *)
	val asserts: b_expr -> t -> bool

	(** [diff p1 p2] returns a list of polyhedra whose union is [p1 \ p2]. *)
	val diff : t -> t -> t list

    (** @return a partition into regions of p. *)
    val get_regions : t -> t list

    (** @return the partition of [p] into two halfs according to the greatest axis-aligned interval. *)
    val split_in_half : t -> t list

    (** Sets up the normalization point used for PLP-based operators. *)
    val set_point : Vector.Rat.t -> t -> t

    (** Returns the size of the given polyhedron. *)
    val size : t -> Scalar.Rat.t option

	(** Computes the effect of parallel assignments on a polyhedron. *)
	(* TODO: Specification of parallel assignments left to define. *)
	val assign: (var * a_expr) list -> t -> t

    val spawn : t -> Vector.Rat.t

    val satisfy : Vector.Rat.t -> t -> bool
end
