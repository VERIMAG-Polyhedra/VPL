open ConsSet
open CstrC
open CstrLCF
open ImpureConfig
open LinTerm
open ProgVar

type t = PedraQOracles.t

val top : t

type 'c pedraCert = (t, Cstr.t, 'c) pedraInput

val isEmpty : 'a1 pedraCert -> 'a1 option Core.Base.imp

val isIncl : ('a1 pedraCert*t) -> (bool*'a1 list) option Core.Base.imp

val add : ('a1 pedraCert*'a1 list) -> (t option*'a1 list) Core.Base.imp

val join :
  ('a1 pedraCert*'a2 pedraCert) -> (t*('a1 list*'a2 list)) Core.Base.imp

val project : ('a1 pedraCert*PVar.t) -> (t*'a1 list) Core.Base.imp

val meet : ('a1 pedraCert*(t*'a1 list)) -> (t option*'a1 list) Core.Base.imp

val rename : ((PVar.t*PVar.t)*t) -> t Core.Base.imp

val widen : (t*t) -> (t*Cs.t) Core.Base.imp

val getItv : ('a1 pedraCert*LinQ.t) -> 'a1 itvT Core.Base.imp

val getUpperBound : ('a1 pedraCert*LinQ.t) -> 'a1 bndT Core.Base.imp

val getLowerBound : ('a1 pedraCert*LinQ.t) -> 'a1 bndT Core.Base.imp

val pr : t -> char list
