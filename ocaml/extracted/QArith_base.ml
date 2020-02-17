open BinInt
open BinNums
open BinPos
open Specif
open ZArith_dec
open Zbool

type coq_Q = { coq_Qnum : coq_Z; coq_Qden : positive }

(** val coq_Qnum : coq_Q -> coq_Z **)

let coq_Qnum x = x.coq_Qnum

(** val coq_Qden : coq_Q -> positive **)

let coq_Qden x = x.coq_Qden

(** val inject_Z : coq_Z -> coq_Q **)

let inject_Z x =
  { coq_Qnum = x; coq_Qden = Coq_xH }

(** val coq_Qeq_dec : coq_Q -> coq_Q -> bool **)

let coq_Qeq_dec x y =
  Z.eq_dec (Z.mul x.coq_Qnum (Zpos y.coq_Qden))
    (Z.mul y.coq_Qnum (Zpos x.coq_Qden))

(** val coq_Qeq_bool : coq_Q -> coq_Q -> bool **)

let coq_Qeq_bool x y =
  coq_Zeq_bool (Z.mul x.coq_Qnum (Zpos y.coq_Qden))
    (Z.mul y.coq_Qnum (Zpos x.coq_Qden))

(** val coq_Qplus : coq_Q -> coq_Q -> coq_Q **)

let coq_Qplus x y =
  { coq_Qnum =
    (Z.add (Z.mul x.coq_Qnum (Zpos y.coq_Qden))
      (Z.mul y.coq_Qnum (Zpos x.coq_Qden))); coq_Qden =
    (Pos.mul x.coq_Qden y.coq_Qden) }

(** val coq_Qmult : coq_Q -> coq_Q -> coq_Q **)

let coq_Qmult x y =
  { coq_Qnum = (Z.mul x.coq_Qnum y.coq_Qnum); coq_Qden =
    (Pos.mul x.coq_Qden y.coq_Qden) }

(** val coq_Qopp : coq_Q -> coq_Q **)

let coq_Qopp x =
  { coq_Qnum = (Z.opp x.coq_Qnum); coq_Qden = x.coq_Qden }

(** val coq_Qminus : coq_Q -> coq_Q -> coq_Q **)

let coq_Qminus x y =
  coq_Qplus x (coq_Qopp y)

(** val coq_Qinv : coq_Q -> coq_Q **)

let coq_Qinv x =
  match x.coq_Qnum with
  | Z0 -> { coq_Qnum = Z0; coq_Qden = Coq_xH }
  | Zpos p -> { coq_Qnum = (Zpos x.coq_Qden); coq_Qden = p }
  | Zneg p -> { coq_Qnum = (Zneg x.coq_Qden); coq_Qden = p }

(** val coq_Q_dec : coq_Q -> coq_Q -> bool sumor **)

let coq_Q_dec x y =
  coq_Z_dec' (Z.mul x.coq_Qnum (Zpos y.coq_Qden))
    (Z.mul y.coq_Qnum (Zpos x.coq_Qden))

(** val coq_Qlt_le_dec : coq_Q -> coq_Q -> bool **)

let coq_Qlt_le_dec x y =
  coq_Z_lt_le_dec (Z.mul x.coq_Qnum (Zpos y.coq_Qden))
    (Z.mul y.coq_Qnum (Zpos x.coq_Qden))
