(** Types of variables.

{ul
    {- {!module:Positive}: Positive integers represented as binary trees}
    {- {!module:Int}: Positive integers representedas machine integers}
    {- {!module:String}: String}
}

VPL main module {!module:Pol} works only with variables of type {!module:Positive},
because the simplex algorithm (implemented in {!module:Splx}) deeply builds on
the internal structure of positive variables.
*)


open Var_type

(** Interface of variables. *)
module type Type = Type

(** Positive variables represent paths in a tree. *)
module Positive : Type with type t = positive

(** Positive variables represented as machine integers. *)
module Int : Type with type t = int

(** Variables represented as strings. *)
module String : Type with type t = string
