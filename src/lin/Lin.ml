module CP = CstrPoly.Positive

module Cert = NCDomain.Factory_Unit

let addPolyM : unit Pol.t -> CP.t list -> unit Pol.t option
	= fun phPol pl ->
	if Pol.equal Cert.factory Cert.factory Pol.top phPol
	then Some phPol
	else
		match !Flags.proj with
		| Flags.Proj_PLP(Flags.Rat) -> (Handelman.Rat.Pb.run phPol pl).Handelman.Rat.Pb.ph#get_vpl_rep
		| Flags.Proj_PLP(Flags.Symbolic)-> (Handelman.Symbolic.Pb.run phPol pl).Handelman.Symbolic.Pb.ph#get_vpl_rep
		| Flags.Proj_PLP(Flags.Float) -> (Handelman.Float.Pb.run phPol pl).Handelman.Float.Pb.ph#get_vpl_rep
		| Flags.FM -> (Handelman.Symbolic.Pb.run phPol pl).Handelman.Symbolic.Pb.ph#get_vpl_rep
	

let addPoly : Cert.t Pol.t -> CP.t -> Cert.t Pol.t option
	= fun phPol p ->
	addPolyM phPol [p]
	
