(** Data structure that hosts the inequalities of a polyhedron.
    It represents only sets of inequalities with {b nonempty interior}, meaning that they must be feasible and they should not contain any implicit equality. *)

(** Type of inequality set. *)
type 'c t = {
    ineqs : 'c Cons.t list; (** Inequalities of the polyhedron *)
    regions : 'c PLP.Region.t list option; (** Regions of a polyhedron *)
}

(** @return true if the polyhedron is top. *)
val is_top : 'c t -> bool

(** Conversion into string. *)
val to_string_raw : 'c t -> string

(** Conversion into string.
    @param varPr the variable printer *)
val to_string: (Var.t -> string) -> 'c t -> string

(** Conversion into string.
    Also prints the certificates.
    @param factory the factory
    @param varPr the variable printer *)
val to_string_ext: 'c Factory.t -> (Var.t -> string) -> 'c t -> string

(** Exception raised when ?*)
exception CertSyn

(** @param factory the factory
    @param cons a constraint
    @param cstr a second constraint
    @return a certificate such that *)
val certSyn: 'c Factory.t -> 'c Cons.t -> Cs.t -> 'c

(** Result of a syntactic inclusion test.
    Certificates are the constraints that must be added to obtain the associated result.*)
type 'c prop_t =
| Empty of 'c
| Trivial
| Implied of 'c
| Check of 'c Cons.t

val synIncl : 'c Factory.t -> 'c EqSet.t -> 'c t -> Cs.t -> 'c prop_t
