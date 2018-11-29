(** Module for objective functions in the PLP.
    An objective function is a combination of the decision of variables, where coefficients are parametric. *)

module Cs = ParamCoeff.Cs

type choiceT
  = OptReached
  | PivotOn of int

val choice_equal : choiceT -> choiceT -> bool

val choice_to_string : choiceT -> string

(** Pivoting strategies of the PLP.
    Only Bland is currently available.*)
type pivotStrgyT = Bland

(** pretty-printer for type [pivotStrgyT] *)
val pivotStrgy_to_string : pivotStrgyT -> string

(** Type of objective function.*)
type t = {
    lin : ParamCoeff.t list; (** Parametric coefficient associated to each decision variable. *)
    cst : ParamCoeff.t; (** Constant part: the current objective value. *)
}

(** An objective function equal to 0. *)
val empty : t

val to_string : t -> string

(** [pr f o] pretty-prints [o], rendering parameter [i] as [f i]. *)
val pr : (int -> string) -> t -> string

(** [prWithVars fp fv o] pretty-prints [o] as a sum of products of parametric coefficients witch variables.
    Note that value of the objective is not displayed.
    @param fv a pretty printer for variables
    @param fp a pretty printer for parameters  *)
val prWithVars : (int -> string) -> (int -> string) -> t -> string

val getColumnsWidth : (int -> string) -> t -> int list

(** Pretty printer of an objective function.
    @param f a pretty printer for parameters
    @param obj the objective function to print
    @param il a list such that the j_th element is the size of column [j] *)
val pretty_print : (int -> string) -> t -> int list -> string

(** Returns the paramatetric coefficient at column j.
    @param j the column
    @param obj the objective function *)
val get : int -> t -> ParamCoeff.t

(** Returns the current objective value. *)
val value : t -> ParamCoeff.t

(** Returns the number of decision variablesin the objective. *)
val nVars : t -> int

(** returns the number of parameters that appear in the objective. *)
val nParams : t -> int

(** Equality test between two objective functions. *)
val equal : t -> t -> bool

(** Builds an objective function.
    @param l the list of parametric coefficients
    @param b the initial objective value.
    @raise Invalid_argument id all the elements of [l] and [b] do not share the same number of parameters *)
val mk : ParamCoeff.t list -> ParamCoeff.t -> t

(** [mkSparse n l b] builds a value of type [t].
The resulting objective has [n] variables, those of which don't have a coefficient in [l] have a nil coefficient.
    @param b is the initial objective value. *)
val mkSparse : int -> (int * ParamCoeff.t) list -> ParamCoeff.t -> t

(** [elim obj vec i] cancels the parametric coefficient at column [i] in [obj], using vector [vec].*)
val elim : t -> Tableau.Vector.t -> int -> t

(** Fold function over the parametric coefficients of an objective function.
This excludes the value of the objective.  For each coefficient, the function is given the column index, starting from 0, and the coefficient value. *)
val foldi : (int -> ParamCoeff.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [add_col o p i] builds a new objective function where coefficient [p] is inserted at position [i], counted from [0].
    It is not supported that [p] refers to parameters which do not appear in [o]. *)
val add_col : t -> ParamCoeff.t -> int -> t

(** Removes the given column form the objective function. *)
val rm_col : t -> int -> t

(** Type of pivots. *)
module type PivotType = sig

    (** The type of vectors used to instantiate the simplex tableau. *)
    module Vec : Vector.Type with module V = Var.Positive and module M = Rtree

    module Naming : Naming.Type with module Vec = Vec

    (** [getPivotCol f h s cx o from_col] returns what the next step is according to
    context [cx] and pivoting strategy [s]. Function [f] defines the correspondence
    between parameter indices and VPL variables. [h] must be greater than any
    [Vec.V.t] returned by [f] and is used to generated internal variables.*)
    val getPivotCol : (int -> Vec.V.t) -> Vec.V.t -> pivotStrgyT -> Naming.t -> Vec.t -> t -> choiceT
end

module Pivot : functor (Vec: Vector.Type with module V = Var.Positive and module M = Rtree) -> PivotType with module Vec = Vec
