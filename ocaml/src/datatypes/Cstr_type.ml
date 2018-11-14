type cmpT_extended
	= EQ | LE | LT | GE | GT | NEQ

type cmpT
	= Eq | Le | Lt

(** Affine constraints and operations on them. *)
module type Type = sig
	module Vec : Vector.Type
    module Coeff : Scalar.Type
    module V : Var.Type

	(** The main type for affine contraints.
	A constraint [c] of type [t] represents [c.v c.typ c.c]. *)
	type t = {
		typ : cmpT; (** the comparison operator *)
		v: Vec.t; (** the linear term *)
		c: Vec.Coeff.t (** the constant *) }

	val name : string

	val get_typ : t -> cmpT
	val get_v : t -> Vec.t
	val get_c : t -> Vec.Coeff.t


	(** [pr b c] pretty-prints contraint [c] using the variables and their names from [b]. *)
	val to_string : (Vec.V.t -> string) -> t -> string

	val list_to_string : t list -> string

	val plot : Vec.V.t list -> t -> string

	val list_plot : t list -> string

	(** The type of properties the can be told by examining a constraint on its own. *)
	type prop_t =
	| Trivial (** 0 = 0 or 0 < 1 are Trivial *)
	| Contrad (** trivialy contradictory: e.g. 0 = 1 or 0 <  -1 *)
	| Nothing (** neither trivial nor trivialy contradictory *)

	(** See [mult]. *)
	exception BadMult

	(** See [elim]. *)
	exception CannotElim

	(** See [elim]. *)
	exception NoElim

	val cmpAdd: cmpT -> cmpT -> cmpT

	(** Compute the complement constraint to the given inequality.
	If the supplied linear constraint is an equality, [Invalid_argument "Cstr.compl"] is raised. *)
	val compl: t -> t

	(** Split the given equality (e.g. x = 1) into the two equivalent inequalities (e.g. x <= 1 and -x <= -1).
	If the supplied constraint is not an equality, [Invalid_argument "Cstr.split"] is raised. *)
	val split: t -> t * t

	(** [mk typ coefs cst] builds the linear constraint [coefs typ cst],
	where each member of [coefs] gives the coefficient of a given variable.
	Zero coefficients can be left implicit. *)
	val mk: cmpT -> (Vec.Coeff.t * Vec.V.t) list -> Vec.Coeff.t -> t

	val mk2: cmpT -> Vec.t -> Vec.Coeff.t -> t

	(** [eq lin cst] builds the equality [lin = cst],
	where each member of [lin] is the coefficient of a variable.
	Zero coefficients can be left implicit. *)
	val eq: (Vec.Coeff.t * Vec.V.t) list -> Vec.Coeff.t -> t

	(** [le lin cst] builds the non-strict inequality [lin <= cst],
	where each member of [lin] is the coefficient of a variable.
	Zero coefficients can be left implicit. *)
	val le: (Vec.Coeff.t * Vec.V.t) list -> Vec.Coeff.t -> t

	(** [lt lin cst] builds the strict inequality [lin < cst],
	where each member of [lin] is the coefficient of a variable.
	Zero coefficients can be left implicit. *)
	val lt: (Vec.Coeff.t * Vec.V.t) list -> Vec.Coeff.t -> t

	(** [mkInt] is equivalent to [mk] but transforms [coefs] < [cst] into [coefs] <= ([cst] - 1). *)
	val mkInt: cmpT -> (Vec.Coeff.t * Vec.V.t) list -> Vec.Coeff.t -> t

	(** Check the syntactical equality of two constraints.
	x = 1 and x = 1 are syntactically equal, but not 2 * x = 2 and x = 1. *)
	val equalSyn: t -> t -> bool

	(** Check the syntactical inclusion of one constraint in another.
	The linear term of both constraints must be syntactically equal.
	x = 0 is included in x < 1, but not is 2 * x < 2. *)
	val inclSyn: t -> t -> bool

	(** [equal c1 c2] checks whether the subspace described by [c1] is equal to that of [c2]. *)
	val equal: t -> t -> bool

	(** [incl c1 c2] checks whether the subspace described by [c1] is included in that of [c2]. *)
	val incl: t -> t -> bool

	(** Add two constraints. *)
	val add: t -> t -> t

	(** Multiply a constraint by a constant.
	If a negative constant is provided and the constraint is an inequality, then [BadMult] is raised. *)
	val mulc: Vec.Coeff.t -> t -> t

	(** Tell what can be infered on a constraint on its own.  See [prop_t]. *)
	val tellProp: t -> prop_t

	(** Build the set of variables which appear with non-zero coefficients in the constraints. *)
	val getVars: t list -> Vec.V.Set.t

	(** [getCoefsFor mx l] gathers the "coefficient" of [mx] in the constraints
	in [l]. If [mx] is [None] then the constants are gathered. If [mx] is
	[Some x], the coefficients of variable [x] are gathered. The order of the
	numbers in the returned list matches that of the constraints in [l]. *)
	val getCoefsFor : Vec.V.t option -> t list -> Vec.Coeff.t list

	(** Syntaxic comparison *)
	val cmp : t -> t -> int

	(** [eval c x] checks that the given point [x] satisfies the constraint [c]. *)
	val eval: t -> Vec.t -> bool

	(** [eval' c point] computes [a.point - b], where [c : ax cmp b]. *)
	val eval': t -> Vec.t -> Vec.Coeff.t

	(** [satisfy vec cstr] returns true if point [vec] satisfies constraint [cstr]. *)
	val satisfy : Vec.t -> t -> bool

	(** [saturate vec cstr] returns true if [vec] satisfies constraint [cstr] where [cstr.typ] has been replaced by {!const:cmpT.Eq}. *)
	val saturate : Vec.t -> t -> bool

	(** Rename fromX toY c *)
	val rename : Vec.V.t -> Vec.V.t -> t -> t

	val rename_f : (Vec.V.t -> Vec.V.t) -> t -> t

	(** [change_variable x lin c cstr] proceeds to the change of variable [x = lin + c] in [cstr]. *)
	val change_variable : Vec.V.t -> Vec.t -> Vec.Coeff.t -> t -> t

	(** Returns a point that saturates the hyperplane defined by the given constraint. *)
	val get_saturating_point : t -> Vec.t

    (** Returns the euclidian distance between the given point and the line defined by the constraint. *)
    val distance_point_cstr : Vec.t -> t -> Vec.Coeff.t

end
