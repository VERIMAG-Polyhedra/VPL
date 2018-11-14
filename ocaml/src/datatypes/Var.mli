(** Most of the VPL modules are functors working with integer of positive variables. These two types of variables are defined here.
VPl works only with positives in modules {!module:Splx}, {!module:Opt}, {!module:IneqSet} and {!module:Pol}.
Even with {!module:Int}, variables are only strictly positive.
*)

(** A value of type [t] identifies a variable and represents a path in a tree.
[XH] is the end of a path, [XO] means "take the left branch" and [XI] means "take the right branch". *)

open Var_type

module type Type = Type

module Int : Type with type t = int

module Positive : Type with type t = positive

module String : Type with type t = string
