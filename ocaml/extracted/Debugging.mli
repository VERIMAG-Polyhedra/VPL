
type failureStatus =
| CERT
| DEMO
| NYI
| INTERN

val failureStatus_rect : 'a1 -> 'a1 -> 'a1 -> 'a1 -> failureStatus -> 'a1

val failureStatus_rec : 'a1 -> 'a1 -> 'a1 -> 'a1 -> failureStatus -> 'a1

type traceStatus =
| OFF
| INFO
| DEBUG

val traceStatus_rect : 'a1 -> 'a1 -> 'a1 -> traceStatus -> 'a1

val traceStatus_rec : 'a1 -> 'a1 -> 'a1 -> traceStatus -> 'a1

val traceCmp : traceStatus -> traceStatus -> bool


