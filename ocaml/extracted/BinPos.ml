open BinNums
open BinPosDef
open Datatypes
open Nat0

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | Coq_xI p -> Coq_xO (succ p)
  | Coq_xO p -> Coq_xI p
  | Coq_xH -> Coq_xO Coq_xH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xI (add p q)
       | Coq_xO q -> Coq_xO (add p q)
       | Coq_xH -> Coq_xI p)
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xO (succ q)
       | Coq_xO q -> Coq_xI q
       | Coq_xH -> Coq_xO Coq_xH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> Coq_xI (add_carry p q)
       | Coq_xO q -> Coq_xO (add_carry p q)
       | Coq_xH -> Coq_xI (succ p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> Coq_xO (add_carry p q)
       | Coq_xO q -> Coq_xI (add p q)
       | Coq_xH -> Coq_xO (succ p))
    | Coq_xH ->
      (match y with
       | Coq_xI q -> Coq_xI (succ q)
       | Coq_xO q -> Coq_xO (succ q)
       | Coq_xH -> Coq_xI Coq_xH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | Coq_xI p -> Coq_xI (Coq_xO p)
  | Coq_xO p -> Coq_xI (pred_double p)
  | Coq_xH -> Coq_xH

  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg

  (** val succ_double_mask : mask -> mask **)

  let succ_double_mask = function
  | IsNul -> IsPos Coq_xH
  | IsPos p -> IsPos (Coq_xI p)
  | IsNeg -> IsNeg

  (** val double_mask : mask -> mask **)

  let double_mask = function
  | IsPos p -> IsPos (Coq_xO p)
  | x0 -> x0

  (** val double_pred_mask : positive -> mask **)

  let double_pred_mask = function
  | Coq_xI p -> IsPos (Coq_xO (Coq_xO p))
  | Coq_xO p -> IsPos (Coq_xO (pred_double p))
  | Coq_xH -> IsNul

  (** val sub_mask : positive -> positive -> mask **)

  let rec sub_mask x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask p q)
       | Coq_xO q -> succ_double_mask (sub_mask p q)
       | Coq_xH -> IsPos (Coq_xO p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xH -> (match y with
                 | Coq_xH -> IsNul
                 | _ -> IsNeg)

  (** val sub_mask_carry : positive -> positive -> mask **)

  and sub_mask_carry x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xO q -> double_mask (sub_mask p q)
       | Coq_xH -> IsPos (pred_double p))
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> double_mask (sub_mask_carry p q)
       | Coq_xO q -> succ_double_mask (sub_mask_carry p q)
       | Coq_xH -> double_pred_mask p)
    | Coq_xH -> IsNeg

  (** val sub : positive -> positive -> positive **)

  let sub x y =
    match sub_mask x y with
    | IsPos z -> z
    | _ -> Coq_xH

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | Coq_xI p -> add y (Coq_xO (mul p y))
    | Coq_xO p -> Coq_xO (mul p y)
    | Coq_xH -> y

  (** val size_nat : positive -> nat **)

  let rec size_nat = function
  | Coq_xI p0 -> S (size_nat p0)
  | Coq_xO p0 -> S (size_nat p0)
  | Coq_xH -> S O

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | Coq_xI p ->
      (match y with
       | Coq_xI q -> compare_cont r p q
       | Coq_xO q -> compare_cont Gt p q
       | Coq_xH -> Gt)
    | Coq_xO p ->
      (match y with
       | Coq_xI q -> compare_cont Lt p q
       | Coq_xO q -> compare_cont r p q
       | Coq_xH -> Gt)
    | Coq_xH -> (match y with
                 | Coq_xH -> r
                 | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val max : positive -> positive -> positive **)

  let max p p' =
    match compare p p' with
    | Gt -> p
    | _ -> p'

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | Coq_xI p0 -> (match q with
                    | Coq_xI q0 -> eqb p0 q0
                    | _ -> false)
    | Coq_xO p0 -> (match q with
                    | Coq_xO q0 -> eqb p0 q0
                    | _ -> false)
    | Coq_xH -> (match q with
                 | Coq_xH -> true
                 | _ -> false)

  (** val leb : positive -> positive -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : positive -> positive -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val ggcdn :
      nat -> positive -> positive -> positive*(positive*positive) **)

  let rec ggcdn n a b =
    match n with
    | O -> Coq_xH,(a,b)
    | S n0 ->
      (match a with
       | Coq_xI a' ->
         (match b with
          | Coq_xI b' ->
            (match compare a' b' with
             | Eq -> a,(Coq_xH,Coq_xH)
             | Lt ->
               let g,p = ggcdn n0 (sub b' a') a in
               let ba,aa = p in g,(aa,(add aa (Coq_xO ba)))
             | Gt ->
               let g,p = ggcdn n0 (sub a' b') b in
               let ab,bb = p in g,((add bb (Coq_xO ab)),bb))
          | Coq_xO b0 ->
            let g,p = ggcdn n0 a b0 in let aa,bb = p in g,(aa,(Coq_xO bb))
          | Coq_xH -> Coq_xH,(a,Coq_xH))
       | Coq_xO a0 ->
         (match b with
          | Coq_xI _ ->
            let g,p = ggcdn n0 a0 b in let aa,bb = p in g,((Coq_xO aa),bb)
          | Coq_xO b0 -> let g,p = ggcdn n0 a0 b0 in (Coq_xO g),p
          | Coq_xH -> Coq_xH,(a,Coq_xH))
       | Coq_xH -> Coq_xH,(Coq_xH,b))

  (** val ggcd : positive -> positive -> positive*(positive*positive) **)

  let ggcd a b =
    ggcdn (Nat0.add (size_nat a) (size_nat b)) a b

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> Coq_xH
  | S x -> succ (of_succ_nat x)

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | Coq_xI p0 -> (match x0 with
                    | Coq_xI p1 -> eq_dec p0 p1
                    | _ -> false)
    | Coq_xO p0 -> (match x0 with
                    | Coq_xO p1 -> eq_dec p0 p1
                    | _ -> false)
    | Coq_xH -> (match x0 with
                 | Coq_xH -> true
                 | _ -> false)
 end
