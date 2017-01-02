open BinNums
open Datatypes

module PositiveSet :
 sig
  type elt = positive

  type tree =
  | Leaf
  | Node of tree * bool * tree

  type t = tree

  val empty : t

  val mem : positive -> t -> bool

  val add : positive -> t -> t

  val node : t -> bool -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val rev_append : elt -> elt -> elt

  val rev : elt -> elt

  val xfold : (positive -> 'a1 -> 'a1) -> t -> 'a1 -> positive -> 'a1

  val fold : (positive -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1
 end
