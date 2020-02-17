
module MPP_Definitions :
 sig
  type 'a coq_MPP =
  | Build_MPP

  val ret : 'a1 -> 'a1 coq_MPP

  val bind : 'a1 coq_MPP -> ('a1 -> 'a2 coq_MPP) -> 'a2 coq_MPP

  val join : 'a1 coq_MPP -> 'a1 coq_MPP -> 'a1 coq_MPP

  val meet : 'a1 coq_MPP -> 'a1 coq_MPP -> 'a1 coq_MPP

  val assume : unit coq_MPP

  val prop : 'a1 coq_MPP

  val coq_try :
    'a1 option -> ('a1 -> 'a2 coq_MPP) -> 'a2 coq_MPP -> 'a2 coq_MPP

  val coq_UMeet : 'a1 coq_MPP

  val coq_UJoin : 'a1 coq_MPP
 end

val coq_Skip : 'a1 -> 'a1 MPP_Definitions.coq_MPP

val coq_Update : ('a1 -> 'a1) -> 'a1 -> 'a1 MPP_Definitions.coq_MPP

val coq_Seq :
  ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a2 -> 'a3
  MPP_Definitions.coq_MPP) -> 'a1 -> 'a3 MPP_Definitions.coq_MPP

val coq_Join :
  ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a1 -> 'a2
  MPP_Definitions.coq_MPP) -> 'a1 -> 'a2 MPP_Definitions.coq_MPP

val coq_Assume : 'a1 -> 'a1 MPP_Definitions.coq_MPP

val coq_Abort : 'a1 -> 'a1 MPP_Definitions.coq_MPP

val coq_Assert : 'a1 -> 'a1 MPP_Definitions.coq_MPP

val coq_Meet :
  ('a1 -> 'a2 MPP_Definitions.coq_MPP) -> ('a1 -> 'a2
  MPP_Definitions.coq_MPP) -> 'a1 -> 'a2 MPP_Definitions.coq_MPP

val coq_UMeet :
  ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> 'a2 -> 'a3
  MPP_Definitions.coq_MPP

val coq_UJoin :
  ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> 'a2 -> 'a3
  MPP_Definitions.coq_MPP

val coq_Try :
  'a1 option -> ('a1 -> 'a2 -> 'a3 MPP_Definitions.coq_MPP) -> ('a2 -> 'a3
  MPP_Definitions.coq_MPP) -> 'a2 -> 'a3 MPP_Definitions.coq_MPP
