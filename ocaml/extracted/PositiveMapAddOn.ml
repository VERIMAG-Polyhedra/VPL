open BinNums
open FMapPositive

(** val single : positive -> 'a1 -> 'a1 PositiveMap.t **)

let rec single x v =
  match x with
  | Coq_xI x' -> PositiveMap.Node (PositiveMap.Leaf, None, (single x' v))
  | Coq_xO x' -> PositiveMap.Node ((single x' v), None, PositiveMap.Leaf)
  | Coq_xH -> PositiveMap.Node (PositiveMap.Leaf, (Some v), PositiveMap.Leaf)

(** val equal :
    ('a1 -> 'a1 -> bool) -> 'a1 PositiveMap.t -> 'a1 PositiveMap.t -> bool **)

let rec equal cmp m1 m2 =
  match m1 with
  | PositiveMap.Leaf ->
    (match m2 with
     | PositiveMap.Leaf -> true
     | PositiveMap.Node (_, _, _) -> false)
  | PositiveMap.Node (l1, o1, r1) ->
    (match m2 with
     | PositiveMap.Leaf -> false
     | PositiveMap.Node (l2, o2, r2) ->
       if if match o1 with
             | Some v1 -> (match o2 with
                           | Some v2 -> cmp v1 v2
                           | None -> false)
             | None -> (match o2 with
                        | Some _ -> false
                        | None -> true)
          then equal cmp l1 l2
          else false
       then equal cmp r1 r2
       else false)

(** val node :
    'a1 PositiveMap.t -> 'a1 option -> 'a1 PositiveMap.t -> 'a1
    PositiveMap.tree **)

let node l o r =
  match l with
  | PositiveMap.Leaf ->
    (match r with
     | PositiveMap.Leaf ->
       (match o with
        | Some _ -> PositiveMap.Node (l, o, r)
        | None -> PositiveMap.Leaf)
     | PositiveMap.Node (_, _, _) -> PositiveMap.Node (l, o, r))
  | PositiveMap.Node (_, _, _) -> PositiveMap.Node (l, o, r)

(** val nodeMerge :
    ('a1 -> 'a1 -> 'a1 option) -> 'a1 option -> 'a1 option -> 'a1 option **)

let nodeMerge f o1 o2 =
  match o1 with
  | Some v1 -> (match o2 with
                | Some v2 -> f v1 v2
                | None -> o1)
  | None -> o2
