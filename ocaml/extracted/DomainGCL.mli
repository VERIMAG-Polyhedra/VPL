open Debugging
open ImpureConfig
open NumC
open PredTrans
open ProgVar
open String0

type __ = Obj.t

module CoreAlarm :
 sig
  module Base :
   sig
    type 'a imp = ('a*bool) Core.Base.imp

    val pure : 'a1 -> ('a1*bool) Core.Base.imp

    val bind : 'a1 imp -> ('a1 -> 'a2 imp) -> ('a2*bool) Core.Base.imp

    val lift : 'a1 Core.Base.imp -> 'a1 imp
   end

  val alarm : char list -> 'a1 -> ('a1*bool) Core.Base.imp
 end

module FullAlarmGCL :
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
 sig
  module DAlarm :
   sig
    type t = D.t

    val isIncl : D.t -> D.t -> bool CoreAlarm.Base.imp

    val top : D.t

    val join : t -> t -> t CoreAlarm.Base.imp

    val widen : t -> t -> t CoreAlarm.Base.imp

    val bottom : D.t

    val isBottom : t -> bool CoreAlarm.Base.imp

    val project : t -> PVar.t -> t CoreAlarm.Base.imp

    val assume : Cond.t -> D.t -> D.t CoreAlarm.Base.imp

    val coq_assert : Cond.t -> D.t -> bool CoreAlarm.Base.imp

    val assign : PVar.t -> Cond.Term.t -> D.t -> D.t CoreAlarm.Base.imp

    val guassign : PVar.t -> Cond.t -> D.t -> D.t CoreAlarm.Base.imp
   end

  type cdac =
    DAlarm.t -> DAlarm.t CoreAlarm.Base.imp
    (* singleton inductive, whose constructor was raw_build_cdac *)

  val impl : cdac -> DAlarm.t -> DAlarm.t CoreAlarm.Base.imp

  val build_cdac :
    (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> (N.t Mem.t -> N.t Mem.t
    MPP_Definitions.coq_MPP) -> cdac

  val spec : cdac -> N.t Mem.t -> N.t Mem.t MPP_Definitions.coq_MPP

  val cast : cdac -> (N.t Mem.t -> N.t Mem.t MPP_Definitions.coq_MPP) -> cdac

  val skip : cdac

  val fail : char list -> cdac

  val seq : cdac -> cdac -> cdac

  val join : cdac -> cdac -> cdac

  val loop : cdac -> (DAlarm.t -> DAlarm.t CoreAlarm.Base.imp) -> cdac

  val skip_info : char list

  val coq_try : 'a1 option -> ('a1 -> cdac) -> cdac

  val bind :
    (DAlarm.t -> 'a1 CoreAlarm.Base.imp) -> ('a1 -> cdac) -> __ -> cdac

  val assume : Cond.t -> cdac

  val assert_msg : char list

  val coq_assert : char list -> Cond.t -> cdac

  val assign : PVar.t -> Cond.Term.t -> cdac

  val guassign : PVar.t -> Cond.t -> cdac
 end
