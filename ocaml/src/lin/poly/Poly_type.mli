(** Interface of polynomials. *)

(** Interface of polynomials.*)
module type Type = sig

    module Vec : Vector.Type

    (** Type of coefficients. *)
	module Coeff = Vec.Coeff

    (** Type of exponent: a nonnegative integer. *)
    type exp = int

	(** MonomialBasis represents the variables of a monomial, i.e. without the coefficient. *)
	module MonomialBasis : sig

        (** Type of monomial basis. *)
		type t = (Var.t * exp) list

        (** Monomial basis with no variable. *)
        val null : t

        (** Builds a monomial basis. *)
        val mk : (Var.t * exp) list -> t

        (** @return the monomial basis *)
        val to_list : t -> (Var.t * exp) list

		(** Pretty-printer for monomial basis.
            @return the monomial basis where variables (which are integers) are prefixed by [s] *)
		val to_string_param : t -> string -> string

		(** Pretty-printer for monomial basis.
            @return the monomial basis where variables (which are integers) are prefixed by "x" *)
		val to_string : t -> string

		(** Lexicographic comparison between two monomial bases.
            @return 0 if [m1] = [m2]
            @return -1 if [m1] < [m2]
            @return 1 otherwise *)
		val compare : t -> t -> int

		(** Equality test between two monomial bases. *)
		val equal : t -> t -> bool

		(** Renames a variable.
            @param m the monomial basis
            @param fromX the variable to rename
            @param toY the new variable name *)
		val rename : t -> Var.t -> Var.t -> t

    	(** Applies the given change of variables on the monomialBasis*)
        val change_variable : (t -> t option) -> t -> t

		(** Evaluates a monomial basis by replacing each variable with its value given by a function.
            @param m the monomial basis
            @param f the variable evalutation *)
		val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

		(** @return true if the degree of [m] is at most one.
		    The monomial basis is assumed to be in canonical form. *)
		val is_linear : t -> bool

        (**/**)
        (** Builds a monomial basis from an expanded list of variables. *)
		val mk_expanded : Var.t list -> t

        (** @return the monomial basis as an expanded list of variables. *)
		val to_list_expanded : t -> Var.t list
        (**/**)
	end

	(** Module of monomials. *)
	module Monomial : sig

        (** Type of monomial*)
		type t = MonomialBasis.t * Coeff.t

        (** pretty-printer for monomials. *)
		val to_string : t -> string

		(** Comparison between two monomials.
            If monomials are equal, compares over coefficients.
            See {!val:MonomialBasis.compare}. *)
		val compare : t -> t -> int

		(** Equality test between two monomials. *)
		val equal : t -> t -> bool

		(** Builds a monomial from a monomialBasis and a coefficient. *)
		val mk : MonomialBasis.t -> Coeff.t -> t

        (** Builds a monomial. *)
        val mk_list : (Var.t * exp) list -> Coeff.t -> t

        (** @returns the monomial. *)
        val data : t -> MonomialBasis.t * Coeff.t

		(** Multiplication of two monomials. *)
		val mul : t -> t -> t

		(** @return true if the degree of [m] is at most one.
    TODO : clarifier
            The monomial is assumed to be in canonical form. *)
		val is_linear : t -> bool

		(** @return true if the degree of [m] is zero.
            The monomial is assumed to be in canonical form.*)
		val is_constant : t -> bool

		(** [eval m v] returns a coefficient which is the evaluation of monomial [m], where each variable is replaced by its value in function [v] *)
		val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

		(** [eval_partial m v] returns a monomial, which is the evaluation of monomial [m] where each variable is replaced by its value in function [v]. If a variable has no value in [v], it remains in the result. *)
		val eval_partial : t -> (Var.t -> Coeff.t option) -> t

		(** Applies the given change of variables on the monomial. *)
		val change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t

        (**/**)
        (** Builds a monomial from an expanded list of variables and a coefficient. *)
		val mk_expanded : Var.t list -> Coeff.t -> t

        val canon : t -> t

        val canonO : t -> t option
        (**/**)
	end

    type t = Monomial.t list

	(** [compare x y] returns 0 if [x] is equal to [y], a negative integer if [x] is smaller than [y], a positive on otherwise *)
	val compare : t -> t -> int

	val to_string : t -> string

	(** [mk l] builds a polynomial from the {!type:Monomial.t} list [l] *)
	val mk : Monomial.t list -> t

    val mk_list : ((Var.t * int) list * Coeff.t) list -> t

	(** [mk2 l] builds a polynomial from the list of Var.t * Coeff.t [l] *)
	val mk_expanded_list : (Var.t list * Coeff.t) list -> t

	val mk_cste : t -> Coeff.t -> t

	val fromVar : Var.t -> t

	(** [data p] returns the polynomial [p] as a {!type:Monomial.t} list *)
	val data : t -> Monomial.t list

	(** [data p] returns the polynomial [p] as a list of Var.t * Coeff.t *)
	val to_list_expanded : t -> (Var.t list * Coeff.t) list

    val canon : t -> t

	(** [cste c] returns the constant polynomial [c] *)
	val cste : Coeff.t -> t

	(** [z] is the null polynomial *)
	val z : t

	(** [u] is the constant polynomial equal to one *)
	val u : t

	(** [negU] is the constant polynomial equal to minus_one *)
	val negU : t

	(** [is_constant p] returns true if [p] is constant *)
	val is_constant : t -> bool

	(** [isZ p] returns true if [p] is null *)
	val isZ : t -> bool

	(** [is_affine p] returns true if [p] is an affine expression.
	[is_affine] assumes that [p] is in canonical form. *)
	val is_affine : t -> bool

	(** [equal p1 p2] returns [true] is [p1 = p2], [false] otherwise *)
	val equal : t -> t -> bool

	(** [add p1 p2] returns the polynomial equal to [p1 + p2] *)
	val add : t -> t -> t

	(** [mul p1 p2] returns the polynomial equal to [p1 * p2] *)
	val mul : t -> t -> t

	(** [mul p1 c] returns the polynomial equal to [p1 * c] *)
	val mulc : t -> Coeff.t -> t

    (** [div p1 p2] returns the polynomial equal to [p1 / p2] if [p2] is constant.
        @raise Div_by_non_constant if [p2] is non-constant.
        *)
    val div : t -> t -> t

	(** [neg p] returns the polynomial equal to [-1*p] *)
	val neg : t -> t

	(** [sub p1 p2] returns the polynomial equal to [p1 - p2] *)
	val sub : t -> t -> t

	(** [sub p1 m] removes the monomial with monomial basis [m] from [p]. *)
	val sub_monomial : t -> MonomialBasis.t -> t

	(** [sum l] returns the sum of every polynomial in the list [l] *)
	val sum : t list -> t

	(** [prod l] returns the product of every polynomial in the list [l] *)
	val prod : t list -> t

	(** [pow p i] returns the polynomial equal to [p ^ i] *)
	val pow : t -> int -> t

	(** [rename p x y] renames each occurency of [x] in [p] by [y] *)
	val rename : t -> Var.t -> Var.t -> t

	(** Applies the given change of variables on the polynomial. *)
	val change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t

	(** [monomial_coefficient p m] returns the scalar coefficient of the monomialBasis [m] in [p] *)
	val monomial_coefficient : t -> MonomialBasis.t -> Coeff.t

	(** [monomial_coefficient_poly p m] returns the polynomial coefficient of monomialBasis [m] in [p].
	For instance, [monomial_coefficient_poly 3x1x2x3 - 2x2x3 {ul [x2;x3]}] = [3x1 - 2]*)
	val monomial_coefficient_poly : t -> MonomialBasis.t -> t

	(** [get_constant p] returns the constant coefficient of polynomial [p] *)
	val get_constant : t -> Coeff.t

	(** [get_affine_part p] returns the affine part of polynomial [p] *)
	val get_affine_part : t -> Var.t list -> t

	(** [get_vars p] returns the list of variables that appear in polynomial [p] *)
	val get_vars : t -> Var.t list

	(* Returns the next unbounded variable in the list of polynomials. *)
	val horizon : t list -> Var.t

	(** [eval p v] returns a coefficient which is the evaluation of polynomial [p], where each variable is replaced by its value in function [v] *)
	val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

	(** [eval_partial p v] returns a polynomial, which is the evaluation of polynomial [p] where each variable is replaced by its value in function [v]. If a variable has no value in [v], it remains in the result. *)
	val eval_partial : t -> (Var.t -> Coeff.t option) -> t

    val gradient : t -> t Rtree.t

	(** [ofCstr vec coeff] builds the polynomial [vec + coeff]. *)
	val ofCstr : Vec.t -> Coeff.t -> t

	(** [toCstr p] returns the linear part and the coefficient of [p].
	@raise Invalid_argument if [p] is not affine. *)
	val toCstr : t -> (Vec.t * Coeff.t)

    val of_string : string -> t

    module Invariant : sig
        module Monom : sig
            val check : Monomial.t -> bool
        end

    val check : t -> bool
    end
end
