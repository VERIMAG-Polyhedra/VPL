open BinNums
open Datatypes

(** val append : positive -> positive -> positive **)

let rec append i j =
  match i with
  | Coq_xI ii -> Coq_xI (append ii j)
  | Coq_xO ii -> Coq_xO (append ii j)
  | Coq_xH -> j

module PositiveMap =
 struct
  type key = positive

  type 'a tree =
  | Leaf
  | Node of 'a tree * 'a option * 'a tree

  type 'a t = 'a tree

  (** val find : key -> 'a1 t -> 'a1 option **)

  let rec find i = function
  | Leaf -> None
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> find ii r
     | Coq_xO ii -> find ii l
     | Coq_xH -> o)

  (** val mem : key -> 'a1 t -> bool **)

  let rec mem i = function
  | Leaf -> false
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> mem ii r
     | Coq_xO ii -> mem ii l
     | Coq_xH -> (match o with
                  | Some _ -> true
                  | None -> false))

  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)

  let rec add i v = function
  | Leaf ->
    (match i with
     | Coq_xI ii -> Node (Leaf, None, (add ii v Leaf))
     | Coq_xO ii -> Node ((add ii v Leaf), None, Leaf)
     | Coq_xH -> Node (Leaf, (Some v), Leaf))
  | Node (l, o, r) ->
    (match i with
     | Coq_xI ii -> Node (l, o, (add ii v r))
     | Coq_xO ii -> Node ((add ii v l), o, r)
     | Coq_xH -> Node (l, (Some v), r))

  (** val remove : key -> 'a1 t -> 'a1 t **)

  let rec remove i m =
    match i with
    | Coq_xI ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match l with
          | Leaf ->
            (match o with
             | Some _ -> Node (l, o, (remove ii r))
             | None ->
               (match remove ii r with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node (Leaf, None, (Node (t0, o0, t1)))))
          | Node (_, _, _) -> Node (l, o, (remove ii r))))
    | Coq_xO ii ->
      (match m with
       | Leaf -> Leaf
       | Node (l, o, r) ->
         (match o with
          | Some _ -> Node ((remove ii l), o, r)
          | None ->
            (match r with
             | Leaf ->
               (match remove ii l with
                | Leaf -> Leaf
                | Node (t0, o0, t1) -> Node ((Node (t0, o0, t1)), None, Leaf))
             | Node (_, _, _) -> Node ((remove ii l), o, r))))
    | Coq_xH ->
      (match m with
       | Leaf -> Leaf
       | Node (l, _, r) ->
         (match l with
          | Leaf ->
            (match r with
             | Leaf -> Leaf
             | Node (_, _, _) -> Node (l, None, r))
          | Node (_, _, _) -> Node (l, None, r)))

  (** val xelements : 'a1 t -> key -> (key*'a1) list **)

  let rec xelements m i =
    match m with
    | Leaf -> []
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         app (xelements l (append i (Coq_xO Coq_xH)))
           ((i,x)::(xelements r (append i (Coq_xI Coq_xH))))
       | None ->
         app (xelements l (append i (Coq_xO Coq_xH)))
           (xelements r (append i (Coq_xI Coq_xH))))

  (** val elements : 'a1 t -> (key*'a1) list **)

  let elements m =
    xelements m Coq_xH

  (** val xfoldi :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> key -> 'a2 **)

  let rec xfoldi f m v i =
    match m with
    | Leaf -> v
    | Node (l, o, r) ->
      (match o with
       | Some x ->
         xfoldi f r (f i x (xfoldi f l v (append i (Coq_xO Coq_xH))))
           (append i (Coq_xI Coq_xH))
       | None ->
         xfoldi f r (xfoldi f l v (append i (Coq_xO Coq_xH)))
           (append i (Coq_xI Coq_xH)))

  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)

  let fold f m i =
    let rec xfoldi0 m0 v i0 =
      match m0 with
      | Leaf -> v
      | Node (l, o, r) ->
        (match o with
         | Some x ->
           xfoldi0 r (f i0 x (xfoldi0 l v (append i0 (Coq_xO Coq_xH))))
             (append i0 (Coq_xI Coq_xH))
         | None ->
           xfoldi0 r (xfoldi0 l v (append i0 (Coq_xO Coq_xH)))
             (append i0 (Coq_xI Coq_xH)))
    in xfoldi0 m i Coq_xH
 end
