open Datatypes
open Specif

(** val lt_eq_lt_dec : nat -> nat -> bool sumor **)

let rec lt_eq_lt_dec n m =
  match n with
  | O -> (match m with
          | O -> Coq_inleft false
          | S _ -> Coq_inleft true)
  | S n0 -> (match m with
             | O -> Coq_inright
             | S m0 -> lt_eq_lt_dec n0 m0)
