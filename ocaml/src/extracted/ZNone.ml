open BinInt
open BinNums

(** val coq_Z_isZero : coq_Z -> bool **)

let coq_Z_isZero = function
| Z0 -> true
| _ -> false

(** val coq_Z_isNat : coq_Z -> bool **)

let coq_Z_isNat = function
| Zneg _ -> false
| _ -> true

(** val coq_Z_isNegNat : coq_Z -> bool **)

let coq_Z_isNegNat = function
| Zpos _ -> false
| _ -> true

module ZN =
 struct
  type t = coq_Z option

  (** val add : t -> t -> t **)

  let add zn1 zn2 =
    match zn1 with
    | Some z1 -> (match zn2 with
                  | Some z2 -> Some (Z.add z1 z2)
                  | None -> None)
    | None -> None

  (** val mulZ1 : coq_Z -> t -> t **)

  let mulZ1 z = function
  | Some z2 -> Some (Z.mul z z2)
  | None -> None

  (** val mulZ : coq_Z -> t -> t **)

  let mulZ z zn =
    if coq_Z_isZero z
    then Some Z0
    else (match zn with
          | Some z2 -> Some (Z.mul z z2)
          | None -> None)

  (** val opp : t -> t **)

  let opp = function
  | Some z -> Some (Z.opp z)
  | None -> None

  (** val isZero : coq_Z option -> bool **)

  let isZero = function
  | Some z -> coq_Z_isZero z
  | None -> false

  (** val join : t -> t -> t **)

  let join zn1 zn2 =
    match zn1 with
    | Some z1 -> (match zn2 with
                  | Some z2 -> Some (Z.max z1 z2)
                  | None -> None)
    | None -> None

  (** val meet : t -> t -> t **)

  let meet zn1 zn2 =
    match zn1 with
    | Some z1 -> (match zn2 with
                  | Some z2 -> Some (Z.min z1 z2)
                  | None -> None)
    | None -> None
 end
