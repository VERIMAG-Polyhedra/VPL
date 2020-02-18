(** This module provides a functor that lifts operators into tracked operators.
    A tracked operator logs its operands into the log file {!val:Config.log_file}.
    The log file can then be executed. *)

module Make : functor (D: AbstractDomain.Type) -> AbstractDomain.Type with
    type var = D.var and
    type a_expr = D.a_expr and
    type b_expr = D.b_expr

open WrapperTraductors

module MakeAbstractDomain : functor (Coeff: Scalar.Type) -> sig
    module Make : functor (D: Interface(Coeff).HighLevelDomain) -> AbstractDomain.Type
        with type a_expr = Interface(Coeff).Term.t
        and type b_expr = Interface(Coeff).Cond.t
        and type var = Var.t
end
