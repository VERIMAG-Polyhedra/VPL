open BinNums
open Datatypes

module PositiveSet =
 struct
  type elt = positive

  type tree =
  | Leaf
  | Node of tree * bool * tree

  type t = tree

  (** val empty : t **)

  let empty =
    Leaf

  (** val mem : positive -> t -> bool **)

  let rec mem i = function
  | Leaf -> false
  | Node (l, o, r) ->
    (match i with
     | Coq_xI i0 -> mem i0 r
     | Coq_xO i0 -> mem i0 l
     | Coq_xH -> o)

  (** val add : positive -> t -> t **)

  let rec add i = function
  | Leaf ->
    (match i with
     | Coq_xI i0 -> Node (Leaf, false, (add i0 Leaf))
     | Coq_xO i0 -> Node ((add i0 Leaf), false, Leaf)
     | Coq_xH -> Node (Leaf, true, Leaf))
  | Node (l, o, r) ->
    (match i with
     | Coq_xI i0 -> Node (l, o, (add i0 r))
     | Coq_xO i0 -> Node ((add i0 l), o, r)
     | Coq_xH -> Node (l, true, r))

  (** val node : t -> bool -> t -> t **)

  let node l b r =
    if b
    then Node (l, b, r)
    else (match l with
          | Leaf ->
            (match r with
             | Leaf -> Leaf
             | Node (_, _, _) -> Node (l, false, r))
          | Node (_, _, _) -> Node (l, false, r))

  (** val inter : t -> t -> t **)

  let rec inter m m' =
    match m with
    | Leaf -> Leaf
    | Node (l, o, r) ->
      (match m' with
       | Leaf -> Leaf
       | Node (l', o', r') ->
         node (inter l l') (if o then o' else false) (inter r r'))

  (** val diff : t -> t -> t **)

  let rec diff m m' =
    match m with
    | Leaf -> Leaf
    | Node (l, o, r) ->
      (match m' with
       | Leaf -> m
       | Node (l', o', r') ->
         node (diff l l') (if o then negb o' else false) (diff r r'))

  (** val rev_append : elt -> elt -> elt **)

  let rec rev_append y x =
    match y with
    | Coq_xI y0 -> rev_append y0 (Coq_xI x)
    | Coq_xO y0 -> rev_append y0 (Coq_xO x)
    | Coq_xH -> x

  (** val rev : elt -> elt **)

  let rev x =
    rev_append x Coq_xH

  (** val xfold : (positive -> 'a1 -> 'a1) -> t -> 'a1 -> positive -> 'a1 **)

  let rec xfold f m v i =
    match m with
    | Leaf -> v
    | Node (l, b, r) ->
      if b
      then xfold f r (f (rev i) (xfold f l v (Coq_xO i))) (Coq_xI i)
      else xfold f r (xfold f l v (Coq_xO i)) (Coq_xI i)

  (** val fold : (positive -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)

  let fold f m i =
    xfold f m i Coq_xH
 end
