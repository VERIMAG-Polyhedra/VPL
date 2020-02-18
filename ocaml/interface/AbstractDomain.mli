(** Type of abstract domains. *)

(** Abstract domain interface *)
module type Type = sig

	(** Type of abstract value. *)
	type t

    (** Type of variables. *)
    type var

    (** Type of certificates. *)
    type cert

    (** Type of arithmetic expressions. *)
    type a_expr

    (** Type of boolean expressions. *)
    type b_expr

	(**/**)
    val var_to_string : var -> string

    val a_expr_to_string : a_expr -> string

    val b_expr_to_string : b_expr -> string
	(**/**)

	(** Unbounded abstract value. *)
	val top: t

	(** Empty abstract value. *)
	val bottom: t

	(** @return true if the given abstract value is empty. *)
	val is_bottom: t -> bool

    (** @return {ul
		{- a unsatisfiability certificate if the given abstract value is bottom}
        {- None otherwise}
	} *)
    val get_bottom_cert : t -> cert option

	(** @return the list of constraints of the abstract value. *)
    val get_cstrs : t -> Cstr.Rat.t list

	(** @return the constraints of the abstract value as a boolean expression. *)
    val get_b_expr : t -> b_expr

	(** @return the list of variables bounded in the abstract value. *)
    val get_vars : t -> var list

	(** @return the intersection of two abstract values. *)
	val meet : t -> t -> t

	(**/**)
	(** @return the minkowski sum of two abstract values. *)
	val minkowski: t -> t -> t
	(**/**)

	(** @return the join of two abstract values. *)
	val join: t -> t -> t

	(** Eliminates the given list of variables from a abstract value, by orthogonal projection.
        This operator relies on a certified single-variable projection.
        Variables will be eliminated one after the other.
        For a projection that eliminates several variables at the same time, see {!val:project_vars}. *)
	val project: var list -> t -> t

    (** Eliminates the given list of variables from a abstract value, by orthogonal projection.
        Contrary to {!val:project}, this operator actually eliminates all variables at the same time. *)
	val project_vars: var list -> t -> t

    (** [proj_incl p1 p2] returns
		{ul
		{- either the projection of [p1] on [p2]'s variables}
        {- None if the projection of [p1] on [p2]'s variables does not contain [p2]}
		} *)
    val proj_incl : t -> t -> t option

    (** Widening operator. *)
	val widen: t -> t -> t

	(** Checks the inclusion of two abstract values. *)
	val leq: t -> t -> bool

	(** @return a string representing the given abstract value.
		Requires a pretty-printer for variables. *)
	val to_string: (var -> string) -> t -> string

	(** @return the upper bound of an expression in an abstract value. *)
	val get_upper_bound : a_expr -> t -> Pol.bndT option

	(** @return the lower bound of an expression in an abstract value. *)
	val get_lower_bound : a_expr -> t -> Pol.bndT option

	(** @return both the upper and lower bounds of an expression in a abstract value. *)
	val itvize : a_expr -> t -> Pol.itvT

	(** Guard operator.
        @return  the intersection between a condition and a abstract value. *)
	val assume: b_expr -> t -> t

	(** @return true if the given condition is satisfied in the given abstract value. *)
	val asserts: b_expr -> t -> bool

	(** [diff p1 p2]
		@return a list of abstract values whose union is [p1 \ p2]. *)
	val diff : t -> t -> t list

    (** @return a partition into regions of the abstract value. *)
    val get_regions : t -> t list

    (** @return the partition of the abstract value into two halfs according to the greatest axis-aligned interval. *)
    val split_in_half : t -> t list

    (** Sets up the normalization point used for PLP-based operators. *)
    val set_point : Vector.Rat.t -> t -> t

    (** @return the size of the given abstract value.
	 	Semantic is left unspecified. *)
    val size : t -> Scalar.Rat.t option

	(** Computes the effect of parallel assignments on a abstract value. *)
	(* TODO: Specification of parallel assignments left to define. *)
	val assign: (var * a_expr) list -> t -> t

	(** @return a point within the abstract value *)
    val spawn : t -> Vector.Rat.t

	(** [satisfy vec p]
		@return true if the point [vec] belongs the the abstract value [p] *)
    val satisfy : Vector.Rat.t -> t -> bool

	(**/**)
	val assume_back : b_expr -> t -> t option
	(**/**)
end
