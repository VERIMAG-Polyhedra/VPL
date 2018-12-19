(** Type of maps from variables to values. *)

open VarMap_type

(** Interface of maps from variables to values. *)
module type Type = Type

(** Functor that takes a variable module, and generates a map. *)
module VarMap : functor (V : Var.Type) -> sig
    include Type with module V = V

    (* To avoid the first parameter in set .*)
	val set2: 'n t -> V.t -> 'n -> 'n t

    val remove : V.t -> 'n t -> 'n t

    val fold2_strict: ('a -> 'n -> 'm -> 'a) -> 'a -> 'n t -> 'm t -> 'a

    val mk2: (V.t * 'n) list -> 'n t
end
