open ConsSet
open CstrC
open CstrLCF
open ImpureConfig
open LinTerm
open ProgVar

type t = PedraQOracles.t

(** val top : t **)

let top = PedraQOracles.top

type 'c pedraCert = (t, Cstr.t, 'c) pedraInput

(** val isEmpty : 'a1 pedraCert -> 'a1 option Core.Base.imp **)

let isEmpty = PedraQOracles.isEmpty

(** val isIncl : ('a1 pedraCert*t) -> (bool*'a1 list) option Core.Base.imp **)

let isIncl = PedraQOracles.isIncl

(** val add :
    ('a1 pedraCert*'a1 list) -> (t option*'a1 list) Core.Base.imp **)

let add = PedraQOracles.add

(** val join :
    ('a1 pedraCert*'a2 pedraCert) -> (t*('a1 list*'a2 list)) Core.Base.imp **)

let join = PedraQOracles.join

(** val project : ('a1 pedraCert*PVar.t) -> (t*'a1 list) Core.Base.imp **)

let project = PedraQOracles.project

(** val meet :
    ('a1 pedraCert*(t*'a1 list)) -> (t option*'a1 list) Core.Base.imp **)

let meet = PedraQOracles.meet

(** val rename : ((PVar.t*PVar.t)*t) -> t Core.Base.imp **)

let rename = PedraQOracles.rename

(** val widen : (t*t) -> (t*Cs.t) Core.Base.imp **)

let widen = PedraQOracles.widen

(** val getItv : ('a1 pedraCert*LinQ.t) -> 'a1 itvT Core.Base.imp **)

let getItv = PedraQOracles.getItv

(** val getUpperBound : ('a1 pedraCert*LinQ.t) -> 'a1 bndT Core.Base.imp **)

let getUpperBound = PedraQOracles.getUpperBound

(** val getLowerBound : ('a1 pedraCert*LinQ.t) -> 'a1 bndT Core.Base.imp **)

let getLowerBound = PedraQOracles.getLowerBound

(** val pr : t -> char list **)

let pr = PedraQOracles.pr
