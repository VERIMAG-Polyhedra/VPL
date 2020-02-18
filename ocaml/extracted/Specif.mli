
type 'a coq_sig = 'a
  (* singleton inductive, whose constructor was exist *)

type 'a sumor =
| Coq_inleft of 'a
| Coq_inright
