module CP = CstrPoly

module F = Handelman.FactoryUnit

let addPolyM' : 'a Factory.t -> 'a Pol.t -> CP.t list -> F.cert Pol.t option
	= fun factory phPol pl ->
	if Pol.equal factory factory Pol.top phPol
	then Some (F.convert phPol)
	else match !Flags.proj with
		| Flags.Proj_PLP(Flags.Rat) -> (Handelman.Rat.Pb.run phPol pl).Handelman.Rat.Pb.ph.vpl_rep
		| Flags.Proj_PLP(Flags.Symbolic)-> (Handelman.Symbolic.Pb.run phPol pl).Handelman.Symbolic.Pb.ph.vpl_rep
		| Flags.Proj_PLP(Flags.Float) -> (Handelman.Float.Pb.run phPol pl).Handelman.Float.Pb.ph.vpl_rep
		| Flags.FM -> (Handelman.Rat.Pb.run phPol pl).Handelman.Rat.Pb.ph.vpl_rep
		| Flags.PHeuristic -> Pervasives.failwith "Lin.addPolyM"


let addPolyM : 'a Factory.t -> 'a Pol.t -> CP.t list -> F.cert Pol.t option
	= fun factory p ->
	Heuristic.apply_proj
		(List.map Cons.get_c (Pol.get_ineqs p))
		(addPolyM' factory p)

let addPoly : 'a Factory.t -> 'a Pol.t -> CP.t -> F.cert Pol.t option
	= fun factory phPol p ->
	addPolyM factory phPol [p]
