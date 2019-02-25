(** Entry point of the VPL. *)

open WrapperTraductors

(** Type for user-defined variables. *)
module type Ident_T = sig
    (** Type of variables *)
	type t

	(** Conversion into VPL variables of type {!val:Var.Positive.t}. *)
	val toVar: t -> Var.t

    (** Conversion from VPL variables. *)
    val ofVar : Var.t -> t

	(** Conversion into string. *)
	val to_string: t -> string
end

(** Exception that can be raised whenever a conversion from a user-defined term into
    VPL terms fails. *)
exception Out_of_Scope

(** Types of domains available in the VPL. *)
module type Domain_T = sig
    (** Type of coefficients used by the domain. *)
    module Coeff : Scalar.Type

    include AbstractDomain.Type with
        type a_expr = Interface(Coeff).Term.t and
        type b_expr = Interface(Coeff).Cond.t and
        type var = Var.t
end

type 'a_expr user_cond =
| Basic of bool
| Atom of 'a_expr * cmpT * 'a_expr
| BinL of 'a_expr user_cond * binl * 'a_expr user_cond
| Not of 'a_expr user_cond

module Make : functor (D : Domain_T) -> AbstractDomain.Type with
    type a_expr = Interface(D.Coeff).Term.t and
    type b_expr = Interface(D.Coeff).Term.t user_cond and
    type var = Var.t

module MakeCustom : functor
    (D : Domain_T)
    (Ident : Ident_T)
    (Expr : sig
        type t
        val to_term: t -> Interface(D.Coeff).Term.t
        val of_term : Interface(D.Coeff).Term.t -> t
    end) -> sig
        include AbstractDomain.Type with
        type a_expr = Expr.t and
        type b_expr = Expr.t user_cond and
        type var = Ident.t

        val to_cond : a_expr user_cond -> Interface(D.Coeff).Cond.t
        val of_cond : Interface(D.Coeff).Cond.t -> a_expr user_cond
    end

module Lift_Ident : functor (I : sig
    type t
	val compare: t -> t -> int
	val to_string: t -> string
    end) -> sig
    include Ident_T with type t = I.t

    val addVars : t list -> unit
    val get_string : Var.t -> string
    val rename : t -> t -> unit
    val remove : t -> unit
end
