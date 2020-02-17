open BinNums
open BinPos

module PVar =
 struct
  type t = positive

  (** val isEq : t -> t -> bool **)

  let isEq =
    Pos.eqb

  (** val eq_dec : t -> t -> bool **)

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

  (** val isLt : t -> t -> bool **)

  let isLt =
    Pos.ltb

  (** val isLe : t -> t -> bool **)

  let isLe =
    Pos.leb

  (** val export : t -> positive **)

  let export x =
    x

  (** val import : positive -> t **)

  let import x =
    x

  (** val max : t -> t -> t **)

  let max =
    Pos.max

  (** val pr : t -> char list **)

  let pr x =
    'v'::(CoqPr.posPr' x)
 end

module Mem =
 struct
  type 'a t = PVar.t -> 'a

  (** val assign : PVar.t -> 'a1 -> 'a1 t -> PVar.t -> 'a1 **)

  let assign x val0 m x' =
    if PVar.eq_dec x x' then val0 else m x'

  (** val lift : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)

  let lift f m x =
    f (m x)
 end
