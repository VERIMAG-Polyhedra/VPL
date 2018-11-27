(** Module for parametric coefficients in the PLP.
    A parametric coefficient is an affine combination of the parameters.
 *)

module Cs = Cstr.Rat.Positive
module Poly : Poly.Type with module Vec.M = Rtree and module Vec.V = Var.Positive and module Vec.Coeff = Scalar.Rat

(** Type of parametric coefficients.*)
type t = {
    lin : Tableau.Vector.t; (** Linear part *)
    cst : Scalar.Rat.t (** Constant part *)
}

(** Parametric coefficient 0. *)
val empty : t

(** Get the linear part of the parametric coefficient *)
val getLin : t -> Tableau.Vector.t

(** Get the constant part of the parametric coefficient *)
val getCst : t -> Scalar.Rat.t

(** Equality predicate for two parametric coefficients *)
val equal : t -> t -> bool

(** Tests if the given parametric coefficient is null. *)
val is_zero : t -> bool

(** Pretty printer for a parametric coefficient.
    @param f a pretty printer for parameters
    @param c the coefficient to print *)
val pr : (int -> string) -> t -> string

(** Default printer for parameters. *)
val paramDfltPr : int -> string

(** pretty-printer for [t] with default pretty-printing for parameters *)
val to_string : t -> string

(** [mk l a] builds a parametric coefficient.
    @param l is the list of coefficients of the parameters, in order.
    @param a is the constant part of the parametric coefficient. *)
val mk : Scalar.Rat.t list -> Scalar.Rat.t -> t

(** [mkSparse n l a] builds a parametric coefficient from a sparse representation.
    Parameters for which there is no coefficient in [l] have zero coefficient.
    @param n is the number of parameters, numbered from [0] to [n - 1]
    @param l is the list of coefficients of the parameters
    @param a is the constant part of the parametric coefficients
    @raise Invalid_argument if [l] has not [n] elements *)
val mkSparse : int -> (int * Scalar.Rat.t) list -> Scalar.Rat.t -> t

(** [mkCst a] is a short-hand for [mkSparse 0 [] a]. *)
val mkCst : Scalar.Rat.t -> t

(** [ofPoly tr n p] builds a parametric coefficient out of a polynomial.
    @param tr handles the mapping between variables and parameter indices
    @param n the number of parameters, numbered from [0] to [n - 1]
    @param p the polynomial to convert
    @raise Invalid_argument if [p] is not an affine expression *)
val ofPoly : (Cs.Vec.V.t -> int) -> int -> Poly.t -> t

(** [toPoly tr c] builds a value of type [PolyQ.t] from a parametric coefficient.
    @param tr is the mapping functions from the indices of the parameters to variables
    @param c the parametric coefficient to convert *)
val toPoly : (int -> Cs.Vec.V.t) -> t -> Poly.t

(** [to_cstr tr cmp c] builds a constraint from a parametric coefficient.
    @param tr is the mapping function from parameter indices to variables
    @param cmp the constraint comparison sign (cannot be NEQ)
    @param c the parametric coefficient to translate
    @raise Invalid_argument if cmp = NEQ *)
val to_cstr : (int -> Cs.Vec.V.t) -> Cstr_type.cmpT_extended -> t -> Cs.t

(** Add two parametric coefficients.
    @raise Invalid_argument if the two parametric coefficients don't have the same size. *)
val add : t -> t -> t

(** Multiply a parametric coefficient by a scalar. *)
val mul : Scalar.Rat.t -> t -> t

val sub : t -> t -> t

(** [is_constant c] returns true is [c] does not depend on any parameter. *)
val is_constant : t -> bool

(** [is_zero c] returns true is [c] has value zero for all parameters value (both linear and constant). *)
val is_zero : t -> bool

(** [nParams c] returns the number of parameters of [c]. *)
val nParams : t -> int

(** [eval c f] computes the value of the parametric coefficient [c], according to the value of parameters in function [f]. *)
val eval : t -> (int -> Q.t) -> Q.t

(** [eval2 c f] computes the value of the coefficient [c] when the value for
the parameters are chosen according to [f]. *)
val eval2 : t -> (int -> Scalar.Symbolic.t) -> Scalar.Symbolic.t
