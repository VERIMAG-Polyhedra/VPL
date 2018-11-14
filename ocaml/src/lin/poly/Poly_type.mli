(** This module type defines the type of polynomials depending on the type of coefficients and variables*)
module type Type = sig
    module Vec : Vector.Type with module M = Rtree and module V = Var.Positive

	module Coeff = Vec.Coeff
    module V = Vec.V

	(** MonomialBasis represents the list of variables of a monomial *)
	module MonomialBasis : sig
		type t = V.t list

		(** [to_string_param m s] returns monomialBasis [m] as a string where variables (which are integers) are prefixed by [s] *)
		val to_string_param : t -> string -> string

		(** [to_string m] returns monomialBasis [m] as a string where variables (which are integers) are prefixed by "x" *)
		val to_string : t -> string

		(** [compare m1 m2] uses lexicographic order to return 0 if [m1] = [m2], a negative integer if [m1] < [m2] and a positive one otherwise *)
		val compare : t -> t -> int

		(** [equal m1 m2] returns [true] if [m1] = [m2], [false] otherwise *)
		val equal : t -> t -> bool

		(** [rename m x y] renames each occurency of [x] in [m] by [y]*)
		val rename : t -> V.t -> V.t -> t

		(** [eval m v] evaluates monomialBasis [m], replacing each variable with its value in function [v] *)
		val eval : t -> (V.t -> Coeff.t) -> Coeff.t

		(** [isLinear m] returns true if the degree of [m] is at most one.
		[isLinear] assumes [m] is in canonical form. *)
		val isLinear : t -> bool

		val mk : V.t list -> t

		val data : t -> V.t list

		val null : t

		(** Applies the given change of variables on the monomialBasis*)
		val change_variable : (t -> t option) -> t -> t
	end

	(** Monomial represents a monomial as a couple of a monomialBasis and a coefficient *)
	module Monomial : sig
		type t = MonomialBasis.t * Vec.Coeff.t

		val to_string : t -> string

		(** [compare x y] returns 0 if [x] is equal to [y], a negative integer if [x] is smaller than [y]
		Use this function to sort the monomials in a polynomial
		This function do NOT compare the monomial coefficients *)
		val compare : t -> t -> int

		(** [monomial_equal (m1,c1) (m2,c2)] returns true if monomials [(m1,c1)] and [(m2,c2)] are equal *)
		val equal : t -> t -> bool

		(** [mk m c] builds a monomial from monomialBasis [m] and coefficient [c] *)
		val mk : MonomialBasis.t -> Coeff.t -> t

		(** [mk2 m c] builds a monomial from V.t list [m] and coefficient [c] *)
		val mk2 : V.t list -> Coeff.t -> t

		val data : t -> MonomialBasis.t * Coeff.t

		(** [mul m1 m2] returns the monomial equal to [m1 * m2] *)
		val mul : t -> t -> t

		(** [isLinear m] returns true if the degree of [m] is one.
		[isLinear] assumes [m] is in canonical form. *)
		val isLinear : t -> bool

		(** [isLinear m] returns true if the degree of [m] is zero.
		[isLinear] assumes [m] is in canonical form. *)
		val isConstant : t -> bool

		(** [eval m v] returns a coefficient which is the evaluation of monomial [m], where each variable is replaced by its value in function [v] *)
		val eval : t -> (V.t -> Coeff.t) -> Coeff.t

		(** [eval_partial m v] returns a monomial, which is the evaluation of monomial [m] where each variable is replaced by its value in function [v]. If a variable has no value in [v], it remains in the result. *)
		val eval_partial : t -> (V.t -> Coeff.t option) -> t

		(** Applies the given change of variables on the monomial. *)
		val change_variable : (MonomialBasis.t -> MonomialBasis.t option) -> t -> t

        val canon : t -> t
        
        val canonO : t -> t option
	end

    type t = Monomial.t list

	(** [compare x y] returns 0 if [x] is equal to [y], a negative integer if [x] is smaller than [y], a positive on otherwise *)
	val compare : t -> t -> int

	val to_string : t -> string

	(** [mk l] builds a polynomial from the {!type:Monomial.t} list [l] *)
	val mk : Monomial.t list -> t

	(** [mk2 l] builds a polynomial from the list of V.t * Coeff.t [l] *)
	val mk2 : (V.t list * Coeff.t) list -> t

	val mk3 : ((V.t * int) list * Coeff.t) list -> t

	val mk_cste : t -> Coeff.t -> t

	(** [mk_cste l c] adds coefficient [c] to the list of V.t * Coeff.t [l] *)
	val mk2_cste : (V.t list * Coeff.t) list -> Coeff.t -> t

	val fromVar : V.t -> t

	(** [data p] returns the polynomial [p] as a {!type:Monomial.t} list *)
	val data : t -> Monomial.t list

	(** [data p] returns the polynomial [p] as a list of V.t * Coeff.t *)
	val data2 : t -> (V.t list * Coeff.t) list

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
	val rename : t -> V.t -> V.t -> t

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
	val get_affine_part : t -> V.t list -> t

	(** [get_vars p] returns the list of variables that appear in polynomial [p] *)
	val get_vars : t -> V.t list

	(* Returns the next unbounded variable in the list of polynomials. *)
	val horizon : t list -> V.t

	(** [eval p v] returns a coefficient which is the evaluation of polynomial [p], where each variable is replaced by its value in function [v] *)
	val eval : t -> (V.t -> Coeff.t) -> Coeff.t

	(** [eval_partial p v] returns a polynomial, which is the evaluation of polynomial [p] where each variable is replaced by its value in function [v]. If a variable has no value in [v], it remains in the result. *)
	val eval_partial : t -> (V.t -> Coeff.t option) -> t

    val gradient : t -> t Vec.M.t

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
