
module MPP_Definitions =
 struct
  type 'a coq_MPP =
  | Build_MPP

  (** val ret : 'a1 -> 'a1 coq_MPP **)

  let ret _ =
    Build_MPP

  (** val bind : 'a1 coq_MPP -> ('a1 -> 'a2 coq_MPP) -> 'a2 coq_MPP **)

  let bind _ _ =
    Build_MPP

  (** val join : 'a1 coq_MPP -> 'a1 coq_MPP -> 'a1 coq_MPP **)

  let join _ _ =
    Build_MPP

  (** val meet : 'a1 coq_MPP -> 'a1 coq_MPP -> 'a1 coq_MPP **)

  let meet _ _ =
    Build_MPP

  (** val assume : unit coq_MPP **)

  let assume =
    Build_MPP

  (** val prop : 'a1 coq_MPP **)

  let prop =
    Build_MPP

  (** val coq_try :
      'a1 option -> ('a1 -> 'a2 coq_MPP) -> 'a2 coq_MPP -> 'a2 coq_MPP **)

  let coq_try _ _ _ =
    Build_MPP

  (** val coq_UMeet : 'a1 coq_MPP **)

  let coq_UMeet =
    Build_MPP

  (** val coq_UJoin : 'a1 coq_MPP **)

  let coq_UJoin =
    Build_MPP
 end

(** val coq_Skip : 'a1 -> 'a1 MPP_Definitions.coq_MPP **)

let coq_Skip =
  MPP_Definitions.ret

(** val coq_Update : ('a1 -> 'a1) -> 'a1 -> 'a1 MPP_Definitions.coq_MPP **)

let coq_Update f m =
  MPP_Definitions.ret (f m)

(** val coq_Seq :
    ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a2 -> 'a3
    MPP_Definitions.coq_MPP) -> 'a1 -> 'a3 MPP_Definitions.coq_MPP **)

let coq_Seq mp1 mp2 m =
  MPP_Definitions.bind (mp1 m) mp2

(** val coq_Join :
    ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a1 -> 'a2
    MPP_Definitions.coq_MPP) -> 'a1 -> 'a2 MPP_Definitions.coq_MPP **)

let coq_Join mp1 mp2 m =
  MPP_Definitions.join (mp1 m) (mp2 m)

(** val coq_Assume : 'a1 -> 'a1 MPP_Definitions.coq_MPP **)

let coq_Assume m =
  let mp2 = MPP_Definitions.ret m in
  MPP_Definitions.bind MPP_Definitions.assume (fun _ -> mp2)

(** val coq_Abort : 'a1 -> 'a1 MPP_Definitions.coq_MPP **)

let coq_Abort _ =
  MPP_Definitions.prop

(** val coq_Assert : 'a1 -> 'a1 MPP_Definitions.coq_MPP **)

let coq_Assert m =
  MPP_Definitions.join MPP_Definitions.prop (MPP_Definitions.ret m)

(** val coq_Meet :
    ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a1 -> 'a2
    MPP_Definitions.coq_MPP) -> 'a1 -> 'a2 MPP_Definitions.coq_MPP **)

let coq_Meet mp1 mp2 m =
  MPP_Definitions.meet (mp1 m) (mp2 m)

(** val coq_UMeet :
    ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> 'a2 -> 'a3
    MPP_Definitions.coq_MPP **)

let coq_UMeet mp m =
  MPP_Definitions.bind MPP_Definitions.coq_UMeet (fun x -> mp x m)

(** val coq_UJoin :
    ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> 'a2 -> 'a3
    MPP_Definitions.coq_MPP **)

let coq_UJoin mp m =
  MPP_Definitions.bind MPP_Definitions.coq_UJoin (fun x -> mp x m)

(** val coq_Try :
    'a1 option -> ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> ('a2 -> 'a3
    MPP_Definitions.coq_MPP) -> 'a2 -> 'a3 MPP_Definitions.coq_MPP **)

let coq_Try o mp1 mp2 m =
  MPP_Definitions.coq_try o (fun x -> mp1 x m) (mp2 m)
