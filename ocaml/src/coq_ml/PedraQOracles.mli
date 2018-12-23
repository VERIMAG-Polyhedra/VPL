(*****************************************)
(* Implementation of the backend API     *)
(* see PedraQBackend.v                   *)
(*                                       *)
(*****************************************)

(* auxiliary functions *)
module Nb = Scalar.Rat
module Vec = Vector.Rat
module Cs = Cstr.Rat

val coqPosToZ: BinNums.positive -> Scalar.Int.t
val zToCoqPos: Scalar.Int.t -> BinNums.positive
val coqZToZ: BinNums.coq_Z -> Scalar.Int.t
val zToCoqZ: Scalar.Int.t -> BinNums.coq_Z
val nToNb: NumC.QNum.t -> Nb.t
val nToNumC: Nb.t -> NumC.QNum.t
val coq_QToNb: QArith_base.coq_Q -> Nb.t
val nToCoq_Q: Nb.t -> QArith_base.coq_Q
val progVarToVar: ProgVar.PVar.t -> Var.t
val varToProgVar: Var.t -> ProgVar.PVar.t
val ltToVec: LinTerm.LinQ.t -> Vec.t
val vecToLt: Vec.t -> LinTerm.LinQ.t
val cToCmpT: NumC.cmpT -> Cstr_type.cmpT
val cToCmp: Cstr_type.cmpT -> NumC.cmpT
val cToCstr: CstrC.Cstr.t -> Cs.t
val cToCstrC: Cs.t -> CstrC.Cstr.t
val cstrCPr: CstrC.Cstr.t -> string

(* PedraQBackend API *)
type t = unit Pol.t
type 'c pedraCert = (t, CstrC.Cstr.t, 'c) CstrLCF.pedraInput

val top: t
val isEmpty: ('c pedraCert) -> ('c option)
val isIncl: ('c pedraCert) * t -> (bool * ('c list)) option
val add: ('c pedraCert) * ('c list) -> (t option) * ('c list)
val join: ('c1 pedraCert) * ('c2 pedraCert) -> (t * (('c1 list) * ('c2 list)))
val meet: ('c pedraCert) * (t * 'c list) -> (t option) * ('c list)
val project: ('c pedraCert) * ProgVar.PVar.t -> t * ('c list)
val rename: (ProgVar.PVar.t * ProgVar.PVar.t) * t -> t
val widen: t * t -> t * ConsSet.Cs.t
val getItv: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.itvT
val getUpperBound: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.bndT
val getLowerBound: 'c pedraCert * LinTerm.LinQ.t -> 'c CstrLCF.bndT
val pr: t -> char list
val export_backend_rep:
  (t*((ProgVar.PVar.t -> ProgVar.PVar.t)*(ProgVar.PVar.t -> ProgVar.PVar.t)))
  -> (unit Pol.t) * (Var.t -> Var.t) * (Var.t -> Var.t)
