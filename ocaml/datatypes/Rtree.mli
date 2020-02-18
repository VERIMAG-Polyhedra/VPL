(** Module of radix trees. *)

type 'n t =
| Nil (** Leaf *)
| Sub of ('n t * 'n * 'n t) (** Left branch, value, right branch. *)

(** Empty map. *)
val empty : 'n t

(** Returns true if the map is empty. *)
val is_empty : 'n t -> bool

(** [get z map v] returns the value associated to [v] in [map]. If there is no such binding, it returns [z]. *)
val get: 'n -> 'n t -> Var.t -> 'n

(** [set z map var value] returns [map], with a binding from [var] to [value].
If the implemented map is {!module:Rtree}, value [z] is placed on each intermediate node. *)
val set: 'n -> 'n t -> Var.t -> 'n -> 'n t

(** [mk z l] builds a map from the list of association [l].
	Is uses {!val:set} that needs a value [z] for the implementation {!module:Rtree}. *)
val mk: 'n -> (Var.t * 'n) list -> 'n t

(** [map f m] applies function [f] for each binding in [m]. *)
val map: ('n -> 'm) -> 'n t -> 'm t

val fold: (Var.t -> 'a -> 'n -> 'a) -> 'a -> 'n t -> 'a

(** Same as {!val:fold}, but with two maps.
Function identity is applied when a binding is missing in one map. *)
val fold2: (Var.t -> 'a -> 'n -> 'm -> 'a) -> 'a -> 'n t -> 'm t -> 'a

(** Same as {!val:fold2}, but the function uses option types. *)
val fold2_opt: (Var.t -> 'a -> 'n option -> 'm option -> 'a) -> 'a -> 'n t -> 'm t -> 'a

(** [find f rt] looks for a node [n] of [rt] for which [f] returns [Some b].
If such a node is found, the path to [n] in [rt] is returned along with the [b] returned by [f].
If [f] returns [None] for all the nodes of [rt], the function [find] returns [None].
The order in which [find] walks through [rt] is not specified. *)
val find: ('a -> 'b option) -> 'a t -> (Var.t * 'b) option

(** Same as {!val:find} with two maps. *)
val find2: (Var.t -> 'm -> 'n -> 'b option) -> 'm t -> 'n t -> (Var.t * 'b) option

(** Same as {!val:find} where the given function is a predicate. *)
val findPred: ('a -> bool) -> 'a t -> (Var.t * 'a) option

(** Same as {!val:find2} where the given function is a predicate. *)
val findPred2: ('a -> 'b -> bool) -> 'a t -> 'b t -> (Var.t * 'a * 'b) option

val toList: 'a t -> (Var.t * 'a) list
val to_string: string -> ('a -> string -> string) -> (Var.t -> string) -> 'a t -> string

(** [mskBuild pred l] builds a mask tree where a node is true
if [pred] has returned [true] for at least one tree in [l] for that [node]. *)
val mskBuild: ('a -> bool) -> 'a t list -> bool t

(** [pathsGet m] returns the set of variables which value is [true] in map [m]. *)
val pathsGet: bool t -> Var.Set.t

(** [basisBuild isNil pr l] builds a [string t] where a path leads to its associated string (provided by [pr])
if there is at least a tree in [l] which associates a non-nil coefficient (determined by [isNil]) to this path. *)
val basisBuild: ('a -> bool) -> (Var.t -> string) -> 'a t list -> string t

val merge : (Var.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

val merge3 : (Var.t -> 'a option -> 'b option -> 'c option -> 'res option) -> 'a t -> 'b t -> 'c t -> 'res t

val for_all2 : ('m option -> 'n option -> bool) -> 'm t -> 'n t -> bool

(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain equal keys and associate them with equal data.
    @param cmp is the equality predicate used to compare the data associated with the keys. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
