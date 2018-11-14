module CP = CstrPoly

val addPolyM : 'a Cert.t -> 'a Pol.t -> CP.t list -> Factory.Unit.t Pol.t option

val addPoly : 'a Cert.t -> 'a Pol.t -> CP.t -> Factory.Unit.t Pol.t option
