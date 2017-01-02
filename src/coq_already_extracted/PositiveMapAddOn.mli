open BinNums
open FMapPositive

val single : positive -> 'a1 -> 'a1 PositiveMap.t

val equal :
  ('a1 -> 'a1 -> bool) -> 'a1 PositiveMap.t -> 'a1 PositiveMap.t -> bool

val node :
  'a1 PositiveMap.t -> 'a1 option -> 'a1 PositiveMap.t -> 'a1 PositiveMap.tree

val nodeMerge :
  ('a1 -> 'a1 -> 'a1 option) -> 'a1 option -> 'a1 option -> 'a1 option
