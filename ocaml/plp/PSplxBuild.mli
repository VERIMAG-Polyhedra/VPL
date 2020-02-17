(** This modules allows to build a simplex tableau. *)

open PSplx

val trivial_constraint : 'c Factory.t -> 'c Cons.t

val update_new_col : int -> (Cs.t -> Scalar.Rat.t) -> 'c t -> unit

(**/**)
module Init : sig
    val init_row : (int -> 'c Cons.t -> Q.t) -> Q.t -> int -> 'c t -> unit
    val init_cstrs : 'c Cons.t list -> 'c PSplx.t -> 'c PSplx.t
    val mk_obj : int list -> 'c PSplx.t -> unit
    val init_var_set : 'c PSplx.t -> unit
    val init_matrix : int -> int -> 'c t -> unit
    val init_new_col : int -> 'c t -> unit
end
(**/**)

(** Initializes a simplex tableau.
    @param cstrs the list of constraint associated with each column
    @param n_rows the number of rows
    @param obj_cstrs the indices of objective constraints in [cstrs]
    @param normalization_point the normalization point *)
val init : 'c Cons.t list -> int -> int list -> Cs.Vec.t -> 'c PSplx.t

(** Adds a constraint to eliminate a variable.
    If the variable does not appear in any constraint, no row is added.
    @param i_row the row index
    @param var the variable to eliminate
    @param i_cstrs the constraint indices from which the variable is eliminated
    @param sx the simplex tableau *)
val elim_from : int -> Var.t -> int list -> PSplx.var_set -> 'c t -> unit

(** Adds an equality constraint between the coefficients of a variable in two sets of constraints.
    If the variable does not appear in any constraint, no row is added.
    @param i_row the row index
    @param var the variable
    @param i_cstrs1 a set of constraints
    @param i_cstrs2 a second set of constraints
    @param sx the simplex tableau *)
val var_equal : int -> Var.t -> int list -> int list -> PSplx.var_set -> 'c PSplx.t -> unit

(** Adds an equality constraint between the constants in two sets of constraints.
    If the variable does not appear in any constraint, no row is added.
    @param i_row the row index
    @param i_cstrs1 a set of constraints
    @param i_cstrs2 a second set of constraints
    @param sx the simplex tableau *)
val cste_equal : int -> int list -> int list -> PSplx.var_set -> 'c PSplx.t -> unit

(** Set the variable set of a row. *)
val set_var_set : PSplx.decision_variable -> PSplx.var_set -> 'c PSplx.t -> unit

(** Adds a new column to the simplex tableau.
    The column is added at the end of the tableau.
    The variable set of the column is necessarily 1. *)
val add_col : 'c Cons.t -> 'c t -> 'c t
