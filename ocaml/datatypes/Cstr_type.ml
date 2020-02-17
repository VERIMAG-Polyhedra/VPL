(** Interface of affine constraints. *)

(** Type of comparators*)
type cmpT
	= Eq | Le | Lt

(** Extended type of comparators. *)
type cmpT_extended
	= EQ | LE | LT | GE | GT | NEQ

(** See {!val:mulc}. *)
exception BadMult

(** Exception that may be raised when trying to eliminate a variable *)
exception CannotElim

(** Exception that may be raised when trying to eliminate a variable *)
exception NoElim

(** Interface of affine constraints. *)
module type Type = sig

    (** A module of vectors. *)
	module Vec : Vector.Type

    (** The coefficients of {!module:Vec}. *)
    module Coeff : Scalar.Type

	(** Type of affine constraints, of the form [c.v c.typ c.c]. *)
	type t = {
		typ : cmpT; (** the comparison operator *)
		v: Vec.t; (** the linear term *)
		c: Vec.Coeff.t (** the constant *) }

    (** Module name. *)
	val name : string

    (** 0 = 0 *)
    val top : t

    (** Returns the contraint comparator. *)
	val get_typ : t -> cmpT

    (** Returns the constraint linear term. *)
	val get_v : t -> Vec.t

    (** Returns the constraint constant term. *)
	val get_c : t -> Vec.Coeff.t

	(** Pretty-printer for constraints.
        @param var_to_string a pretty-printer for variables
        @param cstr the constraint to print *)
	val to_string : (Var.t -> string) -> t -> string

    (** Pretty-printers for lists of constraints. *)
	val list_to_string : t list -> string

    (** Computes the comparator resulting from the addition of two comparators. *)
	val cmpAdd: cmpT -> cmpT -> cmpT

	(** Compute the complement constraint to the given inequality.
	   @raise Invalid_argument if the supplied linear constraint is an equality *)
	val compl: t -> t

	(** Splits the given equality (e.g. x = 1) into the two equivalent inequalities (e.g. x <= 1 and -x <= -1).
	   @raise Invalid_argument if the supplied constraint is not an equality *)
	val split: t -> t * t

	(** Builds a linear constraint
        @param cmp the comparison operator
        @param l the list of pairs (coefficient * variable), representing the
        linear term. Zero coefficients can be left implicit.
        @param cst the constant term *)
	val mk: cmpT -> (Vec.Coeff.t * Var.t) list -> Vec.Coeff.t -> t

    (** Builds a linear constraint
        @param cmp the comparison operator
        @param vec the linear term
        @param cst the constant term *)
	val mk2: cmpT -> Vec.t -> Vec.Coeff.t -> t

	(** Builds an equality.
        @param l the list of pairs (coefficient * variable), representing the
        linear term. Zero coefficients can be left implicit.
        @param cst the constant term *)
	val eq: (Vec.Coeff.t * Var.t) list -> Vec.Coeff.t -> t

	(** Builds a large inequality.
        @param l the list of pairs (coefficient * variable), representing the
        linear term. Zero coefficients can be left implicit.
        @param cst the constant term *)
	val le: (Vec.Coeff.t * Var.t) list -> Vec.Coeff.t -> t

	(** Builds a strict inequality.
        @param l the list of pairs (coefficient * variable), representing the
        linear term. Zero coefficients can be left implicit.
        @param cst the constant term *)
	val lt: (Vec.Coeff.t * Var.t) list -> Vec.Coeff.t -> t

	(** Same as {!val:mk} but transforms [coefs] < [cst] into [coefs] <= ([cst] - 1).
        @param cmp the comparison operator
        @param l the list of pairs (coefficient * variable), representing the
        linear term. Zero coefficients can be left implicit.
        @param cst the constant term *)
	val mkInt: cmpT -> (Vec.Coeff.t * Var.t) list -> Vec.Coeff.t -> t

	(** Checks the syntactical equality between two constraints.
	   [x = 1] and [x = 1] are syntactically equal, but not 2 * x = 2 and x = 1. *)
	val equalSyn: t -> t -> bool

	(** Check the syntactical inclusion of one constraint in another.
	   The linear term of both constraints must be syntactically equal.
	      x = 0 is included in x < 1, but not is 2 * x < 2. *)
	val inclSyn: t -> t -> bool

	(** Checks if two constraints describe the same space. *)
	val equal: t -> t -> bool

	(** Checks if the subspace described by [c1] is included in that of [c2]. *)
	val incl: t -> t -> bool

    (** Comparison between to constraints. *)
	val cmp : t -> t -> int

	(** Adds two constraints. *)
	val add: t -> t -> t

	(** Multiplies a constraint by a constant.
	   @raise BadMult if a negative constant is provided and the constraint is an inequality *)
	val mulc: Vec.Coeff.t -> t -> t

    (** Same as {!val:mulc} but works for negative constants as well. *)
    val mulc_no_exc : Vec.Coeff.t -> t -> t

    (** The type of properties the can be told by examining a constraint on its own. *)
	type prop_t =
	| Trivial (** 0 = 0 or 0 < 1 are Trivial *)
	| Contrad (** trivialy contradictory: e.g. 0 = 1 or 0 <  -1 *)
	| Nothing (** neither trivial nor trivialy contradictory *)

	(** Tells what can be infered on a constraint on its own.
        See {!type:prop_t} *)
	val tellProp: t -> prop_t

	(** Builds the set of variables which appear with non-zero coefficients in the constraints. *)
	val getVars: t list -> Var.Set.t

	(** [getCoefsFor mx l] gathers the "coefficient" of [mx] in the constraints in [l].
        If [mx] is [None] then the constants are gathered.
        If [mx] is [Some x], the coefficients of variable [x] are gathered.
        The order of the numbers in the returned list matches that of the constraints in [l]. *)
	val getCoefsFor : Var.t option -> t list -> Vec.Coeff.t list

	(** [satisfy vec cstr] returns true if point [vec] satisfies constraint [cstr]. *)
	val satisfy : Vec.t -> t -> bool

	(** [saturate vec cstr] returns true if [vec] satisfies constraint [cstr] where [cstr.typ] has been replaced by {!constructor:cmpT.Eq}. *)
	val saturate : Vec.t -> t -> bool

    (** Evaluates a constraint on a point.
        Computes [a.point - b], where [c : ax cmp b]. *)
	val eval: t -> Vec.t -> Vec.Coeff.t

	(** Renames a variable.
        @param fromX the variable to rename
        @param toY the new variable name
        @param cstr the constraint *)
	val rename : Var.t -> Var.t -> t -> t

    (** Renames all variables of a constraint according to a renaming function
        @param f the renaming function
        @param vec the vector to rename *)
	val rename_f : (Var.t -> Var.t) -> t -> t

	(** [change_variable x lin c cstr] applies the change of variable [x = lin + c] in [cstr]. *)
	val change_variable : Var.t -> Vec.t -> Vec.Coeff.t -> t -> t

	(** @return a point that saturates the hyperplane defined by the given constraint. *)
	val get_saturating_point : t -> Vec.t

    (** Returns the euclidian distance between the given point and the line defined by the constraint. *)
    val distance_point_cstr : Vec.t -> t -> Vec.Coeff.t

    (**/*)
	val plot : Var.t list -> t -> string

	val list_plot : t list -> string
    (**/*)
end
