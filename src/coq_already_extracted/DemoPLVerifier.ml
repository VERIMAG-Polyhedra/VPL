open ASCond
open BinNums
open Datatypes
open Debugging
open DomainGCL
open ImpureConfig
open NumC
open PedraZ
open PredTrans
open ProgVar

type __ = Obj.t

type statement =
| Assume of ZCond.cond
| Assert of char list * ZCond.cond
| Assign of positive * ZCond.Term.term
| Guassign of positive * ZCond.cond
| Seq of statement * statement
| Ifs of ZCond.cond * statement * statement
| While of ZCond.cond * statement * ZCond.cond

(** val statement_rect :
    (ZCond.cond -> 'a1) -> (char list -> ZCond.cond -> 'a1) -> (positive ->
    ZCond.Term.term -> 'a1) -> (positive -> ZCond.cond -> 'a1) -> (statement
    -> 'a1 -> statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 ->
    statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 -> ZCond.cond
    -> 'a1) -> statement -> 'a1 **)

let rec statement_rect f f0 f1 f2 f3 f4 f5 = function
| Assume c -> f c
| Assert (msg, c) -> f0 msg c
| Assign (x, t0) -> f1 x t0
| Guassign (x, c) -> f2 x c
| Seq (s1, s2) ->
  f3 s1 (statement_rect f f0 f1 f2 f3 f4 f5 s1) s2
    (statement_rect f f0 f1 f2 f3 f4 f5 s2)
| Ifs (c, s1, s2) ->
  f4 c s1 (statement_rect f f0 f1 f2 f3 f4 f5 s1) s2
    (statement_rect f f0 f1 f2 f3 f4 f5 s2)
| While (c, s0, inv) -> f5 c s0 (statement_rect f f0 f1 f2 f3 f4 f5 s0) inv

(** val statement_rec :
    (ZCond.cond -> 'a1) -> (char list -> ZCond.cond -> 'a1) -> (positive ->
    ZCond.Term.term -> 'a1) -> (positive -> ZCond.cond -> 'a1) -> (statement
    -> 'a1 -> statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 ->
    statement -> 'a1 -> 'a1) -> (ZCond.cond -> statement -> 'a1 -> ZCond.cond
    -> 'a1) -> statement -> 'a1 **)

let rec statement_rec f f0 f1 f2 f3 f4 f5 = function
| Assume c -> f c
| Assert (msg, c) -> f0 msg c
| Assign (x, t0) -> f1 x t0
| Guassign (x, c) -> f2 x c
| Seq (s1, s2) ->
  f3 s1 (statement_rec f f0 f1 f2 f3 f4 f5 s1) s2
    (statement_rec f f0 f1 f2 f3 f4 f5 s2)
| Ifs (c, s1, s2) ->
  f4 c s1 (statement_rec f f0 f1 f2 f3 f4 f5 s1) s2
    (statement_rec f f0 f1 f2 f3 f4 f5 s2)
| While (c, s0, inv) -> f5 c s0 (statement_rec f f0 f1 f2 f3 f4 f5 s0) inv

(** val sub : ZCond.Term.term -> ZCond.Term.term -> ZCond.Term.term **)

let sub x y =
  ZCond.Term.Add (x, (ZCond.Term.Opp y))

module G = FullAlarmGCL(ZNum)(ZCond)(FullDom)

(** val embed : ZCond.t -> FullDom.t CoreAlarm.Base.imp **)

let embed c =
  G.DAlarm.assume c FullDom.top

(** val postfinder : statement -> G.cdac **)

let rec postfinder = function
| Assume c -> G.assume c
| Assert (msg, c) -> G.coq_assert msg c
| Assign (x, t0) -> G.assign x t0
| Guassign (x, c) -> G.guassign x c
| Seq (s1, s2) -> G.seq (postfinder s1) (postfinder s2)
| Ifs (c, s1, s2) ->
  G.join (G.seq (G.assume c) (postfinder s1))
    (G.seq (G.assume (ZCond.Not c)) (postfinder s2))
| While (c, s0, inv) ->
  G.seq (G.loop (G.seq (G.assume c) (postfinder s0)) (fun _ -> embed inv))
    (G.assume (ZCond.Not c))

(** val postfinderX : statement -> G.cdac **)

let postfinderX s =
  G.cast (postfinder s)
    (coq_UJoin (fun m' ->
      coq_Seq coq_Assume
        (coq_Try m' (fun m0 -> coq_Update (fun _ -> m0)) coq_Abort)))

(** val verifier : statement -> bool Core.Base.imp **)

let verifier s =
  let p = G.impl (postfinderX s) FullDom.top in
  if FullDom.isBottom (fst p)
  then (fun mode l a -> if (Debugging.traceCmp INFO mode) then (print_string (CoqPr.charListTr l); print_newline()); a)
         INFO
         ('*'::('*'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('a'::('b'::('s'::('u'::('r'::('d'::(' '::('p'::('o'::('s'::('t'::('c'::('o'::('n'::('d'::('i'::('t'::('i'::('o'::('n'::(' '::('!'::(' '::('*'::('*'::[]))))))))))))))))))))))))))))))))))
         (snd p)
  else snd p
