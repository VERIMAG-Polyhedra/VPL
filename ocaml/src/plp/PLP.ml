module Cs = Cstr.Rat.Positive

module Debug = DebugTypes.Debug(struct let name = "PLP" end)

module type Type = sig
	module Minimization : Min.Type
	module MapV : Map.S with type key = int
	module PSplx : PSplx.Type
	module Naming : Naming.Type

	(** This map is used for the certificate generation.
	Is associates to each simplex column an element of {!type:Cons.t}. *)
	type 'c mapVar_t = ('c Cons.t) MapV.t

	module Boundary : sig
		type t
	end

	module Region : sig
		type t = {
			id : int;
			r : (Boundary.t * int option) list;
			point : Minimization.VecInput.t; (* Un point dans la région *)
			sx : PSplx.t option(* Tableau de simplexe dont l'objectif a donné cette région *)
		}
	end

	type region_t = Cone | NCone

	module ExplorationPoint : sig
		type t =
			| Direction of int * Boundary.t (** (id of origin region, boundary*)
			| Point of Minimization.VecInput.t
	end

	type t

	val get_cert_default : 'c Cert.t -> 'c mapVar_t -> PSplx.t -> 'c

	val get_no_cert : 'c Cert.t -> PSplx.t -> 'c

    type config = {
		add_region : Region.t option -> Region.t -> ExplorationPoint.t -> t -> t;
		reg_t : region_t;
		points : ExplorationPoint.t list;
		stgy : Objective.pivotStrgyT;
		regions : Region.t list;
		}

	val std_config : config

	val run : config -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option

	(** Same as {!val:run} where regions are cones. *)
	val run_classic : PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
end

module PLP(Minimization : Min.Type) = struct

	module Minimization = Minimization
	include PLPDistrib.PLP(Minimization)

	let get_cert_default : 'c Cert.t -> 'c mapVar_t -> PSplx.t -> 'c
		= fun factory map sx ->
		let basisValue = PSplx.getCurVal sx in
		List.fold_left
			(fun l (col,q) ->
				if Q.equal q Q.zero
				then l
				else
					try let (_,cert) = MapV.find col map in
						(cert, q) :: l
					with Not_found -> l)
			[]
			basisValue
		|> Cert.linear_combination factory

	let get_no_cert : 'c Cert.t -> PSplx.t -> 'c
		= fun factory _ ->
		factory.Cert.top

	let standard : Region.t option -> Region.t -> ExplorationPoint.t -> t -> t
			= fun origin_reg reg point plp ->
			Debug.log DebugTypes.Normal (lazy "Adding new region to plp");
			let id = reg.Region.id in
			let regs = MapV.add id reg plp.regs in
			let new_points = extract_points reg id in
			match point with
			| ExplorationPoint.Point _ as point ->
				let todo = new_points @ (Add_Region.remove_point point plp.todo) in
				{regs = regs ; todo = todo}
			| ExplorationPoint.Direction (_, (cstr, _)) as point ->
				let todo = match origin_reg with
					| Some origin_reg ->
						if Add_Region.should_explore_again origin_reg cstr reg
						then new_points @ plp.todo
						else new_points @ (Add_Region.remove_point point plp.todo)
					| None -> new_points @ plp.todo
				in
				{regs = regs ; todo = todo}

	let standard_test : Region.t option -> Region.t -> ExplorationPoint.t -> t -> t
			= fun _ reg point plp ->
			Debug.log DebugTypes.Normal (lazy "Adding new region to plp");
			let regs = MapV.add reg.Region.id reg plp.regs in
			let todo = Add_Region.remove_point point plp.todo in
			{regs = regs ; todo = todo}

	let std_config = {
		add_region = standard;
		reg_t = Cone;
		points = [];
		stgy = Objective.Bland;
		regions = [];
	}

	let run: config -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
		= fun config sx get_cert ->
		let res =
			if PSplx.nParams sx = 0 || !Flags.distributed_plp = None || !Flags.distributed_plp = Some 0
			then run config sx get_cert
			else Pervasives.failwith "Distributed PLP not implemented" (*Distributed.run config.stgy sx get_cert*)
		in begin
			if PLPCore.Debug.is_enabled DebugTypes.Sage
			then match res with
				| None -> ()
				| Some regs -> Plot.plot regs
		end;
		res

	let run_classic : PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
		= fun sx get_cert ->
		run std_config sx get_cert

end
