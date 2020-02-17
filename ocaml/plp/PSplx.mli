(** This module defines the type of parametric simplex tableau.
    In VPL, the PLP is always use to express a combination of some input linear constraints. This specificity is thus put  directly in the simplex tableau.
    Each column of the tableau is associated with an input constraint (with a certificate).
    Constraints of the problem are expressed as functions of these constraints. *)


module Debug : DebugTypes.Type

module Cs = Cstr.Rat

(** Each column of the simplex tableau is associated with a decision variable.
Therefore, decision variables are identified by their column position in the simplex tableau. *)
type decision_variable = int

(** Decision variables can be given a set of variables, with which they are allowed to pivot.
    By default, variables are given the set [1]. *)
type var_set = int

module VarMap : Map.S with type key = decision_variable

(** This type is used to replay pivots when adding new columns.
    It represents a pivot operation.
    @param cstr the objective coefficient
    @param i_col the index of the new column
    @param tab the simplex tableau on which the pivot must be applied
    @return the new objective coefficient *)
type pivotT = Cs.t * int * Tableau.t -> Cs.t

(** Type of simplex tableau.
    All fields that may change during a pivoting operation are mutable. *)
type 'c t = {
    mutable obj : Objective.t; (** Objective function *)
    mutable tab : Tableau.t; (** Constraint matrix *)
    mutable get_set : var_set VarMap.t; (** map associating decision variables to their set *)
    mutable basis : int list;
    mutable sets : var_set list; (** Associates a var_set to each row *)
    cstrs : 'c Cons.t list; (** The constraint associated to each variable. *)
    mutable new_col : (Cs.t -> Scalar.Rat.t) array;
    mutable pivots : pivotT list; (** List of pivots performed on this tableau. *)
}

(** Empty simplex tableau. *)
val empty : 'c t

val copy : 'c t -> 'c t

val copy_in : 'c t -> 'c t -> unit

(** Conversion intro string. *)
val to_string : 'c t -> string

(** Prints the simplex tableau in the standard output. *)
val print : 'c t -> unit

(** @return the number of decision variables of the problem. *)
val nVars : 'c t -> int

(** @return the number of parameters of the problem. *)
val nParams : 'c t -> int

(** @return the number of rows of the problem. *)
val nRows : 'c t -> int

(** @return the set of parameters of the problem. *)
val getParams : 'c t -> Var.Set.t

(** @return the current value of the objective function, expressed as a constraint over the parameters. *)
val objValue : 'c t -> Cs.t

val objValueCert : 'c Factory.t -> 'c t -> 'c Cons.t

val constant_index : 'c t -> int

(** [getCurVal sx] returns the current value of the basic variables in [sx].
The value of the other variables is implicitly zero.  Each basic variable
is given its value as a pair (column number, value) *)
val getCurVal : 'c t -> (int * Q.t) list

(** @return true is the basic solution is feasible, i.e. if all variables have a nonnegative value.
    The basic solution consists in setting all non-basic variables to zero and setting each basic variable to the right-hand side of the constraint it appears on. *)
val isFeasible : 'c t -> bool

val remCol : int -> 'c t -> 'c t
