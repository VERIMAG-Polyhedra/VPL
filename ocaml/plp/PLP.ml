include PLPIncremental

let standard : 'c Region.t option -> 'c Region.t -> ExplorationPoint.t -> 'c t -> 'c t
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

let standard_test : 'c Region.t option -> 'c Region.t -> ExplorationPoint.t -> 'c t -> 'c t
		= fun _ reg point plp ->
		Debug.log DebugTypes.Normal (lazy "Adding new region to plp");
		let regs = MapV.add reg.Region.id reg plp.regs in
		let todo = Add_Region.remove_point point plp.todo in
		{regs = regs ; todo = todo}

let std_config = {
	add_region = standard;
	reg_t = Cone;
	points = [];
	regions = [];
}

let run_classic : 'c Factory.t -> 'c PSplx.t -> ('c Region.t * 'c Cons.t) list option
	= fun factory sx ->
	run factory std_config sx
