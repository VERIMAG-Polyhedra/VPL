open BinInt
open BinNums
open BinPos
open Datatypes

type 'c coq_Pol =
| Pc of 'c
| Pinj of positive * 'c coq_Pol
| PX of 'c coq_Pol * positive * 'c coq_Pol

val coq_P0 : 'a1 -> 'a1 coq_Pol

val coq_P1 : 'a1 -> 'a1 coq_Pol

val coq_Peq : ('a1 -> 'a1 -> bool) -> 'a1 coq_Pol -> 'a1 coq_Pol -> bool

val mkPinj : positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val mkPinj_pred : positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val mkPX :
  'a1 -> ('a1 -> 'a1 -> bool) -> 'a1 coq_Pol -> positive -> 'a1 coq_Pol ->
  'a1 coq_Pol

val mkXi : 'a1 -> 'a1 -> positive -> 'a1 coq_Pol

val mkX : 'a1 -> 'a1 -> 'a1 coq_Pol

val coq_Popp : ('a1 -> 'a1) -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_PaddC : ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Pol -> 'a1 -> 'a1 coq_Pol

val coq_PsubC : ('a1 -> 'a1 -> 'a1) -> 'a1 coq_Pol -> 'a1 -> 'a1 coq_Pol

val coq_PaddI :
  ('a1 -> 'a1 -> 'a1) -> ('a1 coq_Pol -> 'a1 coq_Pol -> 'a1 coq_Pol) -> 'a1
  coq_Pol -> positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_PsubI :
  ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1) -> ('a1 coq_Pol -> 'a1 coq_Pol -> 'a1
  coq_Pol) -> 'a1 coq_Pol -> positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_PaddX :
  'a1 -> ('a1 -> 'a1 -> bool) -> ('a1 coq_Pol -> 'a1 coq_Pol -> 'a1 coq_Pol)
  -> 'a1 coq_Pol -> positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_PsubX :
  'a1 -> ('a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> ('a1 coq_Pol -> 'a1 coq_Pol
  -> 'a1 coq_Pol) -> 'a1 coq_Pol -> positive -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_Padd :
  'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> 'a1 coq_Pol -> 'a1
  coq_Pol -> 'a1 coq_Pol

val coq_Psub :
  'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1) -> ('a1
  -> 'a1 -> bool) -> 'a1 coq_Pol -> 'a1 coq_Pol -> 'a1 coq_Pol

val coq_PmulC_aux :
  'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> 'a1 coq_Pol -> 'a1 ->
  'a1 coq_Pol

val coq_PmulC :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> 'a1 coq_Pol ->
  'a1 -> 'a1 coq_Pol

val coq_PmulI :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> ('a1 coq_Pol
  -> 'a1 coq_Pol -> 'a1 coq_Pol) -> 'a1 coq_Pol -> positive -> 'a1 coq_Pol ->
  'a1 coq_Pol

val coq_Pmul :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  bool) -> 'a1 coq_Pol -> 'a1 coq_Pol -> 'a1 coq_Pol

type 'c coq_PExpr =
| PEO
| PEI
| PEc of 'c
| PEX of positive
| PEadd of 'c coq_PExpr * 'c coq_PExpr
| PEsub of 'c coq_PExpr * 'c coq_PExpr
| PEmul of 'c coq_PExpr * 'c coq_PExpr
| PEopp of 'c coq_PExpr
| PEpow of 'c coq_PExpr * coq_N

val mk_X : 'a1 -> 'a1 -> positive -> 'a1 coq_Pol

val coq_Ppow_pos :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  bool) -> ('a1 coq_Pol -> 'a1 coq_Pol) -> 'a1 coq_Pol -> 'a1 coq_Pol ->
  positive -> 'a1 coq_Pol

val coq_Ppow_N :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  bool) -> ('a1 coq_Pol -> 'a1 coq_Pol) -> 'a1 coq_Pol -> coq_N -> 'a1 coq_Pol

val norm_aux :
  'a1 -> 'a1 -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 -> 'a1) -> ('a1 -> 'a1 ->
  'a1) -> ('a1 -> 'a1) -> ('a1 -> 'a1 -> bool) -> 'a1 coq_PExpr -> 'a1 coq_Pol
