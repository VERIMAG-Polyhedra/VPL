module CP = CstrPoly

module F = FactoryMaker.Make(FactoryMaker.Unit)

let addPolyM' : 'a Factory.t -> 'a Pol.t -> CP.t list -> F.cert Pol.t option
	= fun factory phPol _ ->
	if Pol.equal factory factory Pol.top phPol
	then Some (F.convert phPol)
	else failwith "Handelman linearization is not currently available"
	(*(Handelman.Rat.Pb.run phPol pl).Handelman.Rat.Pb.ph.vpl_rep*)


let addPolyM : 'a Factory.t -> 'a Pol.t -> CP.t list -> F.cert Pol.t option
	= fun factory p ->
	Heuristic.apply_proj
		(List.map Cons.get_c (Pol.get_ineqs p))
		(addPolyM' factory p)

let addPoly : 'a Factory.t -> 'a Pol.t -> CP.t -> F.cert Pol.t option
	= fun factory phPol p ->
	addPolyM factory phPol [p]
