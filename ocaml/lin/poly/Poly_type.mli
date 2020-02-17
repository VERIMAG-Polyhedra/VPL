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

        (** Adds a variable to an already built monomial basis.
            If the variable is already present, exponents are summed. *)
        val add_var : (Var.t * exp) -> t -> t

        (** Removes a variable (whatever its exponent) from a monomial basis. *)
        val remove_var : Var.t -> t -> t

        (** Removes the given exponent of the variable in the monomial basis.*)
        val remove_var_exp : Var.t -> int -> t -> t

        (** @return the monomial basis *)
        val to_list : t -> (Var.t * exp) list

		(** Pretty-printer for monomial basis.
            @return the monomial basis where variables (which are integers) are prefixed by [s] *)
		val to_string_param : t -> string -> string

		(** Pretty-printer for monomial basis.
            @return the monomial basis where variables (which are integers) are prefixed by "x" *)
		val to_string : t -> string

        (** @return the exponent of the given variable. *)
        val get_exponent : Var.t -> t -> int

		(** Lexicographic comparison between two monomial bases.
            @return 0 if [m1] = [m2]
            @return -1 if [m1] < [m2]
            @return 1 otherwise *)
		val compare : t -> t -> int

		(** Equality test between two monomial bases. *)
		val equal : t -> t -> bool

        (** Removes variables from a monomial basis.
            @param m1 the monomial to substract from
            @param m2 the monomial to substract
            @return true if [m2] is a subset of [m1] and m1 - m2 makes sense*)
        val sub : t -> t -> (t * bool)

		(** Renames a variable.
            @param m the monomial basis
            @param fromX the variable to rename
            @param toY the new variable name *)
		val rename : t -> Var.t -> Var.t -> t

    	(** Applies the given change of variables on the monomialBasis*)
        val change_variable : (t -> t option) -> t -> t

		(** Evaluates a monomial basis with an evaluation function
            @param m the monomial basis
            @param f the evaluation function *)
		val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

        (** @return the set of variables of the monomial basis. *)
        val get_vars : t -> Var.Set.t

        (** @return the degree of the monomial. *)
        val degree : t -> int

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

        (** Zero monomial. *)
        val null : t

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

        (** @return the monomial. *)
        val data : t -> MonomialBasis.t * Coeff.t

        (** @return the exponent of the given variable. *)
        val get_exponent : Var.t -> t -> int

		(** Multiplication of two monomials. *)
		val mul : t -> t -> t

		(** @return true if the degree of [m] is at most one.
            The monomial is assumed to be in canonical form. *)
		val is_linear : t -> bool

		(** @return true if the degree of [m] is zero.
            The monomial is assumed to be in canonical form.*)
		val is_constant : t -> bool

		(** Evaluates a monomial with an evaluation function
            @param m the monomial to evaluate
            @param f the evaluation function *)
        val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

		(** Evaluates a monomial with a partial evaluation function.
            If a variable has no value in [f], it remains in the result.
            @param m the monomial to evaluate
            @param f the partial evaluation function *)
		val eval_partial : t -> (Var.t -> Coeff.t option) -> t

        (** @return the set of variables of the monomial. *)
        val get_vars : t -> Var.Set.t

		(** Applies the given change of variables on the monomial. *)
		val change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t

        (**/**)
        (** Builds a monomial from an expanded list of variables and a coefficient. *)
		val mk_expanded : Var.t list -> Coeff.t -> t

        val canon : t -> t

        val canonO : t -> t option
        (**/**)
	end

    (** Type of polynomials. *)
    type t = Monomial.t list

    (** Pretty-printer for polynomials. *)
	val to_string : t -> string

	(** Builds a polynomial from a list of {!type:Monomial.t}. *)
	val mk : Monomial.t list -> t

    (** Builds a polynomial from a list of monomials. *)
    val mk_list : ((Var.t * int) list * Coeff.t) list -> t

    (** Builds a polynomial from a single variable. *)
	val fromVar : Var.t -> t

    (** Builds a polynomial from a constant. *)
	val cste : Coeff.t -> t

    (** Builds the polynomial form a constraints.
        @param vec the linear part of the constraint
        @param cste the constant part of the constraint
        @return the polynomial [vec + coeff] *)
	val ofCstr : Vec.t -> Coeff.t -> t

	(** @return the linear part and the coefficient of the polynomial.
        @raise Invalid_argument if the polynomial is not linear. *)
	val toCstr : t -> (Vec.t * Coeff.t)

    (** Builds a polynomial from a string. *)
    val of_string : string -> t

	(** @return the polynomial [p] as a {!type:Monomial.t} list *)
	val data : t -> Monomial.t list

    (** Comparison between two polynomials.
        If monomials are equal, compares over coefficients.
        See {!val:MonomialBasis.compare}. *)
	val compare : t -> t -> int

	(** Constant polynomial equal to zero. *)
	val z : t

	(** Constant polynomial equal to one. *)
	val u : t

	(** Constant polynomial equal to minus_one. *)
	val negU : t

	(** @return true if the polynomial is constant *)
	val is_constant : t -> bool

	(** @return true if the polynomial is zero. *)
	val isZ : t -> bool

	(** @return true if the polynomial is linear (i.e. degre <= 1).
	   The input polynomial is assumed to be in canonical form. *)
	val is_linear : t -> bool

	(** Equality test between two polynomials. *)
	val equal : t -> t -> bool

    (** Adds a constant to a polynomial. *)
    val add_cste : t -> Coeff.t -> t

	(** Addition of two polynomials. *)
	val add : t -> t -> t

	(**Product of two polynomials. *)
	val mul : t -> t -> t

	(** Product of a polynomial by a constant. *)
	val mulc : t -> Coeff.t -> t

    (** Division of a polynomial by another.
        @raise Div_by_non_constant if [p2] is non-constant *)
    val div : t -> t -> t

	(** @return the negation of the polynomial *)
	val neg : t -> t

	(** Substraction of two polynomials. *)
	val sub : t -> t -> t

	(** Removes a monomial from a polynomial.
        @param the polynomial
        @param the monomial basis to remove
        The coefficient is not watched, only the variables. *)
	val sub_monomial : t -> MonomialBasis.t -> t

	(** Sum a list of polynomials. *)
	val sum : t list -> t

	(** Product of a list of polynomials. *)
	val prod : t list -> t

	(** Power of a polynomial by an integer. *)
	val pow : t -> int -> t

    (** @return the maximum exponent of the variable in the polynomial. *)
    val get_max_exponent : Var.t -> t -> int

	(** Renames a variable in the polynomial.
        @param p the polynomial
        @param fromX the variable to rename
        @param toY the new variable name *)
	val rename : t -> Var.t -> Var.t -> t

	(** Applies the given change of variables on the polynomial. *)
	val change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t

	(** @return the scalar coefficient of the given monomialBasis in the polynomial. *)
	val monomial_coefficient : t -> MonomialBasis.t -> Coeff.t

	(** @return the polynomial coefficient of the given monomialBasis in [p].
	For instance, [monomial_coefficient_poly 3x1x2x3 - 2x2x3 {ul [x2;x3]}] = [3x1 - 2]*)
	val monomial_coefficient_poly : t -> MonomialBasis.t -> t

	(** @return the constant coefficient of the polynomial. *)
	val get_constant : t -> Coeff.t

	(** @return the linear part of the polynomial.
        It means all terms that are linear according to {!val:is_linear}. *)
	val get_linear_part : t -> Var.t list -> t

	(** @return the set of variables of the polynomial. *)
	val get_vars : t -> Var.Set.t

	(** @return the next unbounded variable in the list of polynomials. *)
	val horizon : t list -> Var.t

	(** Evaluates a polynomial by replacing each variable with its value given by a function.
        @param p the polynomial to evaluate
        @param f the evaluation function *)
	val eval : t -> (Var.t -> Coeff.t) -> Coeff.t

    (** Evaluates a polynomial with a partial evaluation function.
        If a variable has no value in [f], it remains in the result.
        @param p the polynomial to evaluate
        @param f the partial evaluation function *)
	val eval_partial : t -> (Var.t -> Coeff.t option) -> t

    (** Computes the gradient of the polynomial.
        The result if a map associating to each variable its partial derivative. *)
    val gradient : t -> t Rtree.t

    (**/**)
    module Invariant : sig
        module Monom : sig
            val check : Monomial.t -> bool
        end

        val check : t -> bool
    end

	val mk_expanded_list : (Var.t list * Coeff.t) list -> t

    val to_list_expanded : t -> (Var.t list * Coeff.t) list

    val canon : t -> t
    (**/**)
end
