
type failureStatus =
| CERT
| DEMO
| NYI
| INTERN

(** val failureStatus_rect :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> failureStatus -> 'a1 **)

let failureStatus_rect f f0 f1 f2 = function
| CERT -> f
| DEMO -> f0
| NYI -> f1
| INTERN -> f2

(** val failureStatus_rec :
    'a1 -> 'a1 -> 'a1 -> 'a1 -> failureStatus -> 'a1 **)

let failureStatus_rec f f0 f1 f2 = function
| CERT -> f
| DEMO -> f0
| NYI -> f1
| INTERN -> f2

type traceStatus =
| OFF
| INFO
| DEBUG

(** val traceStatus_rect : 'a1 -> 'a1 -> 'a1 -> traceStatus -> 'a1 **)

let traceStatus_rect f f0 f1 = function
| OFF -> f
| INFO -> f0
| DEBUG -> f1

(** val traceStatus_rec : 'a1 -> 'a1 -> 'a1 -> traceStatus -> 'a1 **)

let traceStatus_rec f f0 f1 = function
| OFF -> f
| INFO -> f0
| DEBUG -> f1

(** val traceCmp : traceStatus -> traceStatus -> bool **)

let traceCmp global local =
  match global with
  | OFF -> false
  | INFO -> (match local with
             | INFO -> true
             | _ -> false)
  | DEBUG -> (match local with
              | OFF -> false
              | _ -> true)


