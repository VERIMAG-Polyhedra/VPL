(** This module adds a name to each abstract value.
    All abstract domain operators are lifted accordingly.
*)

(** When an operator raises an exception, it is catched and handled by function {!val:report}.
Exception ReportHandled is raised afterwards. *)
exception ReportHandled

type 'a named_of = {
    value: 'a;
    name: string;
}

module Make : functor (D : AbstractDomain.Type) -> AbstractDomain.Type with 
    type t = D.t named_of and
    type var = D.var and
    type a_expr = D.a_expr and
    type b_expr = D.b_expr
