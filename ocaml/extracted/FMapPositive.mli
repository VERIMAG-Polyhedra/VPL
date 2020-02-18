open BinNums
open Datatypes

val append : positive -> positive -> positive

module PositiveMap :
 sig
  type key = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  val find : key -> 'a1 t -> 'a1 option

  val mem : key -> 'a1 t -> bool

  val add : key -> 'a1 -> 'a1 t -> 'a1 t

  val remove : key -> 'a1 t -> 'a1 t

  val xelements : 'a1 t -> key -> (key*'a1) list

  val elements : 'a1 t -> (key*'a1) list

  val xfoldi : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2

  val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2
 end
