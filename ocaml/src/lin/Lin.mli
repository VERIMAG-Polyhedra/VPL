module CP = CstrPoly

val addPolyM : 'a Factory.t -> 'a Pol.t -> CP.t list -> Handelman.FactoryUnit.cert Pol.t option

val addPoly : 'a Factory.t -> 'a Pol.t -> CP.t -> Handelman.FactoryUnit.cert Pol.t option
