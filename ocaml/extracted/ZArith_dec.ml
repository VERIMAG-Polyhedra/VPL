open BinInt
open BinNums
open Datatypes
open Specif

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_Zcompare_rect :
    coq_Z -> coq_Z -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

let coq_Zcompare_rect n m h1 h2 h3 =
  let c = Z.compare n m in
  (match c with
   | Eq -> h1 __
   | Lt -> h2 __
   | Gt -> h3 __)

(** val coq_Zcompare_rec :
    coq_Z -> coq_Z -> (__ -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)

let coq_Zcompare_rec =
  coq_Zcompare_rect

(** val coq_Z_lt_dec : coq_Z -> coq_Z -> bool **)

let coq_Z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val coq_Z_lt_ge_dec : coq_Z -> coq_Z -> bool **)

let coq_Z_lt_ge_dec =
  coq_Z_lt_dec

(** val coq_Z_lt_le_dec : coq_Z -> coq_Z -> bool **)

let coq_Z_lt_le_dec =
  coq_Z_lt_ge_dec

(** val coq_Z_le_lt_eq_dec : coq_Z -> coq_Z -> bool **)

let coq_Z_le_lt_eq_dec x y =
  coq_Zcompare_rec x y (fun _ -> false) (fun _ -> true) (fun _ ->
    assert false (* absurd case *))

(** val not_Zeq_inf : coq_Z -> coq_Z -> bool **)

let not_Zeq_inf x y =
  if coq_Z_lt_ge_dec x y
  then true
  else if coq_Z_le_lt_eq_dec y x
       then false
       else assert false (* absurd case *)

(** val coq_Z_dec' : coq_Z -> coq_Z -> bool sumor **)

let coq_Z_dec' x y =
  if Z.eq_dec x y then Coq_inright else Coq_inleft (not_Zeq_inf x y)
