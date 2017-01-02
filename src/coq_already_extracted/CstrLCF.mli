open NumC

type ('e, 'c) cstrLCF = { top : 'c; triv : (cmpT -> QNum.t -> 'c);
                          add : ('c -> 'c -> 'c); mul : (QNum.t -> 'c -> 'c);
                          merge : ('c -> 'c -> 'c); to_le : ('c -> 'c);
                          export : ('c -> 'e) }

type ('t, 'e, 'c) pedraInput = { lcf : ('e, 'c) cstrLCF; backend : 't;
                                 cert : 'c list }

type 'c bndT =
| Infty
| Open of QNum.t * 'c
| Closed of QNum.t * 'c

type 'c itvT = { low : 'c bndT; up : 'c bndT }

val low : 'a1 itvT -> 'a1 bndT

val up : 'a1 itvT -> 'a1 bndT
