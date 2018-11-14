module Cs = Cstr.Rat.Positive

module type Type = sig
    module Vec : Vector.Type with module M = Rtree and module V = Var.Positive
    module Pivot : Objective.PivotType with module Vec = Vec
    module Naming = Pivot.Naming

    type t
      = {
      obj : Objective.t;
      mat : Tableau.Matrix.t;
      basis : int list;
      names : Naming.t
    }

    val empty : t

    val get_obj : t -> Objective.t
    val get_mat : t -> Tableau.Matrix.t
    val get_basis : t -> int list

    val to_string : t -> string

    val nRows : t -> int
    val nCols : t -> int
    val nVars : t -> int
    val nParams : t -> int

    val getVars : t -> Vec.V.t list
    val getParams : t -> Vec.V.t list

    val obj_value : t -> ParamCoeff.t
    val obj_value' : t -> Cs.t

    (** [getCurVal sx] returns the current value of the basic variables in [sx].
    The value of the other variables is implicitly zero.  Each basic variable
    is given its value as a pair (column number, value) *)
    val getCurVal : t -> (int * Q.t) list

    (** [equal sx sx'] returns true if [sx] and [sx'] are
    syntactically equal. *)
    val equal : t -> t -> bool

    (** [isFeasible sx] returns true is the basic solution is feasible,
    that is all variables have a non-negative value.  The basic solution
    consists in setting all the non-basic variables to zero and setting
    each basic variable to the right-hand side of the constraint it appears
    on. *)
    val isFeasible : t -> bool

    (** [addSlackAt i sx] adds a slack variable on row [i] of [sx].
    The slack variable is added as an extra column at the right of the
    tableau. *)
    val addSlackAt : int -> t -> t

    (** [addSlacks n sx] adds slack variables for the first [n] rows
    of the tableau. *)
    val addSlacks : int -> t -> t

    val get_row_pivot : Tableau.Matrix.t -> int -> int

    (** [pivot sx row col] performs a pivot between [row] and [col]. *)
    val pivot : t -> int -> int -> t

    module Diag : sig

      (** [isCanon sx] returns true if the t tableau [sx] is in canonical form.
    What is checked is that each row of the matrix of constraints has a designated
    basic variable which appears only on that row. *)
      val isCanon : t -> bool

    end

    module Explore : sig
        module Init : sig
        (** Module [Init] gathers all the functions necessary to find an
        initial feasible basis for a parametric linear problem.
        XXX: it uses an internal variable [a] which [VariablesInt.t] identifier is [-1].
        *)

          val getReplacementForA : t -> int -> int
          val buildInitFeasibilityPb : t -> t
          val buildFeasibleTab : Objective.t -> t -> t
          val correction : t -> t option
          val findFeasibleBasis : t -> Vec.t -> t option
        end

        val push : Objective.pivotStrgyT -> Vec.t -> t -> t
        val init_and_push : Objective.pivotStrgyT -> Vec.t -> t -> t option
    end

    module Build : sig
        module Poly = ParamCoeff.Poly

        (** [from_poly vars ineqs eqs obj] builds the parametric simplex tableau with constraints [ineqs @ eqs], objective [obj] and where simplex variables are in [vars].
        @param ineqs represent polynomials of the form [p <= 0].*)
        val from_poly : Poly.V.t list -> Poly.t list -> Poly.t list -> Poly.t -> t
    end
end
