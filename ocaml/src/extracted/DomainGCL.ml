open Debugging
open ImpureConfig
open NumC
open PredTrans
open ProgVar
open String0

type __ = Obj.t

module CoreAlarm =
 struct
  module Base =
   struct
    type 'a imp = ('a*bool) Core.Base.imp

    (** val pure : 'a1 -> ('a1*bool) Core.Base.imp **)

    let pure a =
      a,true

    (** val bind : 'a1 imp -> ('a1 -> 'a2 imp) -> ('a2*bool) Core.Base.imp **)

    let bind k1 k2 =
      let a1,b1 = k1 in let a2,b2 = k2 a1 in a2,(if b1 then b2 else false)

    (** val lift : 'a1 Core.Base.imp -> 'a1 imp **)

    let lift k =
      k,true
   end

  (** val alarm : char list -> 'a1 -> ('a1*bool) Core.Base.imp **)

  let alarm msg a =
    let a0 =
      (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
        INFO msg a
    in
    a0,false
 end

module FullAlarmGCL =
 functor (N:NumSig) ->
 functor (Cond:sig
  module Term :
   sig
    type t

    val eval : t -> N.t Mem.t -> N.t
   end

  type t
 end) ->
 functor (D:sig
  type t

  val isIncl : t -> t -> bool Core.Base.imp

  val top : t

  val join : t -> t -> t Core.Base.imp

  val widen : t -> t -> t Core.Base.imp

  val bottom : t

  val isBottom : t -> bool Core.Base.imp

  val project : t -> PVar.t -> t Core.Base.imp

  val assume : Cond.t -> t -> t Core.Base.imp

  val coq_assert : Cond.t -> t -> bool Core.Base.imp

  val assign : PVar.t -> Cond.Term.t -> t -> t Core.Base.imp

  val guassign : PVar.t -> Cond.t -> t -> t Core.Base.imp
 end) ->
 struct
  module DAlarm =
   struct
    type t = D.t

    (** val isIncl : D.t -> D.t -> bool CoreAlarm.Base.imp **)

    let isIncl a a' =
      CoreAlarm.Base.lift (D.isIncl a a')

    (** val top : D.t **)

    let top =
      D.top

    (** val join : t -> t -> t CoreAlarm.Base.imp **)

    let join a1 a2 =
      CoreAlarm.Base.lift (D.join a1 a2)

    (** val widen : t -> t -> t CoreAlarm.Base.imp **)

    let widen a1 a2 =
      CoreAlarm.Base.lift (D.widen a1 a2)

    (** val bottom : D.t **)

    let bottom =
      D.bottom

    (** val isBottom : t -> bool CoreAlarm.Base.imp **)

    let isBottom a =
      CoreAlarm.Base.lift (D.isBottom a)

    (** val project : t -> PVar.t -> t CoreAlarm.Base.imp **)

    let project a x =
      CoreAlarm.Base.lift (D.project a x)

    (** val assume : Cond.t -> D.t -> D.t CoreAlarm.Base.imp **)

    let assume c a =
      CoreAlarm.Base.lift (D.assume c a)

    (** val coq_assert : Cond.t -> D.t -> bool CoreAlarm.Base.imp **)

    let coq_assert c a =
      CoreAlarm.Base.lift (D.coq_assert c a)

    (** val assign :
        PVar.t -> Cond.Term.t -> D.t -> D.t CoreAlarm.Base.imp **)

    let assign x t0 a =
      CoreAlarm.Base.lift (D.assign x t0 a)

    (** val guassign : PVar.t -> Cond.t -> D.t -> D.t CoreAlarm.Base.imp **)

    let guassign x c a =
      CoreAlarm.Base.lift (D.guassign x c a)
   end

  type cdac =
    DAlarm.t -> DAlarm.t CoreAlarm.Base.imp
    (* singleton inductive, whose constructor was raw_build_cdac *)

  (** val impl : cdac -> DAlarm.t -> DAlarm.t CoreAlarm.Base.imp **)

  let impl c =
    c

  (** val build_cdac :
      (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> (N.t Mem.t -> N.t Mem.t
      MPP_Definitions.coq_MPP) -> cdac **)

  let build_cdac impl0 _ =
    impl0

  (** val spec : cdac -> N.t Mem.t -> N.t Mem.t MPP_Definitions.coq_MPP **)

  let spec _ _ =
    MPP_Definitions.Build_MPP

  (** val cast :
      cdac -> (N.t Mem.t -> N.t Mem.t MPP_Definitions.coq_MPP) -> cdac **)

  let cast gc spec' =
    build_cdac (impl gc) spec'

  (** val skip : cdac **)

  let skip: cdac =
    build_cdac CoreAlarm.Base.pure coq_Skip

  (** val fail : char list -> cdac **)

  let fail msg =
    build_cdac (fun a ->
      CoreAlarm.Base.pure
        ((fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
          DEMO msg a)) coq_Skip

  (** val seq : cdac -> cdac -> cdac **)

  let seq gc1 gc2 =
    build_cdac (fun a ->
      CoreAlarm.Base.bind (impl gc1 a) (fun a' ->
        CoreAlarm.Base.bind (DAlarm.isBottom a') (fun b ->
          if b then CoreAlarm.Base.pure a' else impl gc2 a')))
      (coq_Seq (spec gc1) (spec gc2))

  (** val join : cdac -> cdac -> cdac **)

  let join gc1 gc2 =
    build_cdac (fun a ->
      CoreAlarm.Base.bind (impl gc1 a) (fun a1 ->
        CoreAlarm.Base.bind (impl gc2 a) (fun a2 -> DAlarm.join a1 a2)))
      (coq_Join (spec gc1) (spec gc2))

  (** val loop : cdac -> (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> cdac **)

  let loop gc oracle =
    build_cdac (fun a ->
      CoreAlarm.Base.bind (oracle a) (fun inv ->
        CoreAlarm.Base.bind (DAlarm.isIncl a inv) (fun b ->
          if b
          then CoreAlarm.Base.bind (impl gc inv) (fun a' ->
                 CoreAlarm.Base.bind (DAlarm.isIncl a' inv) (fun b' ->
                   if b'
                   then CoreAlarm.Base.pure inv
                   else let inv0 =
                          (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                            DEMO
                            ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('p'::('r'::('e'::('s'::('e'::('r'::('v'::('a'::('t'::('i'::('o'::('n'::[]))))))))))))))))))))))
                            DAlarm.top
                        in
                        CoreAlarm.Base.bind (impl gc inv0) (fun _ ->
                          CoreAlarm.Base.pure inv0)))
          else let inv0 =
                 (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
                   DEMO
                   ('i'::('n'::('v'::('a'::('r'::('i'::('a'::('n'::('t'::(' '::('i'::('n'::('i'::('t'::[]))))))))))))))
                   DAlarm.top
               in
               CoreAlarm.Base.bind (impl gc inv0) (fun _ ->
                 CoreAlarm.Base.pure inv0))))
      (coq_UMeet (fun _ ->
        coq_Seq coq_Assert
          (coq_Seq coq_Assert
            (coq_UJoin (fun m ->
              coq_Seq (coq_Update (fun _ -> m)) coq_Assume)))))

  (** val skip_info : char list **)

  let skip_info =
    '#'::('s'::('k'::('i'::('p'::('#'::[])))))

  (** val coq_try : 'a1 option -> ('a1 -> cdac) -> cdac **)

  let coq_try o gc =
    build_cdac (fun a ->
      match o with
      | Some x -> impl (gc x) a
      | None ->
        let a0 =
          (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
            INFO skip_info a
        in
        CoreAlarm.Base.pure a0) (coq_Try o (fun x -> spec (gc x)) coq_Skip)

  (** val bind :
      (DAlarm.t -> 'a1 CoreAlarm.Base.imp) -> ('a1 -> cdac) -> __ -> cdac **)

  let bind ge gc _ =
    build_cdac (fun a -> CoreAlarm.Base.bind (ge a) (fun x -> impl (gc x) a))
      (coq_UMeet (fun x -> coq_Seq coq_Assert (spec (gc x))))

  (** val assume : Cond.t -> cdac **)

  let assume cond =
    build_cdac (DAlarm.assume cond) coq_Assume

  (** val assert_msg : char list **)

  let assert_msg =
    'f'::('a'::('i'::('l'::('e'::('d'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(':'::[])))))))))))))

  (** val coq_assert : char list -> Cond.t -> cdac **)

  let coq_assert msg cond =
    build_cdac (fun a ->
      let a0 =
        (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
          DEBUG
          (append
            ('C'::('h'::('e'::('c'::('k'::('i'::('n'::('g'::(' '::('a'::('s'::('s'::('e'::('r'::('t'::(' '::[]))))))))))))))))
            msg) a
      in
      CoreAlarm.Base.bind (DAlarm.coq_assert cond a0) (fun b ->
        if b
        then CoreAlarm.Base.pure a0
        else CoreAlarm.alarm (append assert_msg msg) a0)) coq_Assert

  (** val assign : PVar.t -> Cond.Term.t -> cdac **)

  let assign x te =
    build_cdac (DAlarm.assign x te)
      (coq_Update (fun m -> Mem.assign x (Cond.Term.eval te m) m))

  (** val guassign : PVar.t -> Cond.t -> cdac **)

  let guassign x c =
    build_cdac (DAlarm.guassign x c)
      (coq_UJoin (fun v -> coq_Seq coq_Assume (coq_Update (Mem.assign x v))))
 end
