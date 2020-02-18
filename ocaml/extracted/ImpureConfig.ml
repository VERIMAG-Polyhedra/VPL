open Impure

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Core =
 struct
  module Base =
   struct
    type 'a imp = 'a

    (** val pure : 'a1 -> 'a1 **)

    let pure a =
      a

    (** val bind : 'a1 -> ('a1 -> 'a2) -> 'a2 **)

    let bind k1 k2 =
      k2 k1
   end

  (** val impeq_bind_pure_l : __ **)

  let impeq_bind_pure_l =
    __

  (** val impeq_bind_pure_r : __ **)

  let impeq_bind_pure_r =
    __

  (** val impeq_bind_assoc : __ **)

  let impeq_bind_assoc =
    __

  type wlp = __

  (** val wlp_compat_Proper : __ **)

  let wlp_compat_Proper =
    __

  (** val wlp_compat : __ **)

  let wlp_compat =
    __

  (** val wlp_unfold : __ **)

  let wlp_unfold =
    __

  (** val wlp_monotone : __ **)

  let wlp_monotone =
    __

  (** val wlp_forall : __ **)

  let wlp_forall =
    __

  (** val wlp_pure : __ **)

  let wlp_pure =
    __

  (** val wlp_bind : __ **)

  let wlp_bind =
    __

  (** val coq_List_fold_left_impeq_run : __ **)

  let coq_List_fold_left_impeq_run =
    __

  (** val wlp_List_fold_left : __ **)

  let wlp_List_fold_left =
    __
 end
