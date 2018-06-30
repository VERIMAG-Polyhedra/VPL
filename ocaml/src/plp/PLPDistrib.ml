module Cs = PLPCore.Cs
module EqSet = PLPCore.EqSet
module Cons = PLPCore.Cons

module DebugMaster = DebugTypes.Debug(struct let name = "PLP_Master" end)
module DebugSlave = DebugTypes.Debug(struct let name = "PLP_Slave" end)

module PLP(Minimization : Min.Type) = struct

	include PLPPlot.Plot(Minimization)

    (** index of the process in the list of processes.*)
    type global_region_idT = int
    type local_region_idT = int

	(*XXX : le parsing ne fonctionne pas quand il n'y a pas de paramètres *)
	module Distributed = struct

        (* Parameters of the problem. *)
        (* TODO : ne pas oublier de l'initialiser !*)
        let params : Vec.V.t list ref = ref []
        let naming : Naming.t ref = ref Naming.empty

        let getRank : unit -> Mpi.rank
            = fun () ->
            Mpi.comm_rank Mpi.comm_world

        module Print = struct

			let vector : Vec.t -> string
				= fun vec ->
				String.concat " "
				(List.map
					(fun param ->
						Vec.get vec param |> Vec.Coeff.plp_print)
				!params)

			let paramCoeff : int -> ParamCoeff.t -> string
				= fun nParams p ->
				let lin_str =
					if nParams = 0
					then "0"
					else String.concat " " (List.map Q.to_string p.ParamCoeff.lin)
				in
				Printf.sprintf "%s %s"
					lin_str
					(Q.to_string p.ParamCoeff.cst)

			let obj : int -> Objective.t -> string
				= fun nParams obj ->
				Printf.sprintf "%s\n%s"
					(String.concat "\n" (List.map (paramCoeff nParams) obj.Objective.lin))
					((paramCoeff nParams) obj.Objective.cst)

			(** Format :
			s n_vars n_params
			basis
			variables
			parameters
			obj
			m
			matrix
			*)
			let sx : PSplx.t -> string
			= fun sx ->
			let vars = List.map (fun i -> Naming.to_user sx.PSplx.names Naming.Var i |> fst)
				(Misc.range 0 (PSplx.nVars sx))
			and params = List.map (fun i -> Naming.to_user sx.PSplx.names Naming.Param i |> fst)
				(Misc.range 0 (PSplx.nParams sx) )
			in
			Printf.sprintf "simplex %i %i\n%s\n%s\n%s\n%s\nmatrix\n%s\n"
				(PSplx.nVars sx)
				(PSplx.nParams sx)
				(String.concat " " (List.map string_of_int sx.PSplx.basis))
				(String.concat " " (List.map Vec.V.plp_print vars))
				(String.concat " " (List.map Vec.V.plp_print params))
				((obj (PSplx.nParams sx)) sx.PSplx.obj)
				(String.concat "\n" (List.map
					(fun v -> String.concat " " (List.map Q.to_string v))
					sx.PSplx.mat))

			let cstr : Cs.t -> string
				= fun cstr ->
				let vec = Cs.get_v cstr in
				Printf.sprintf "%s %s"
				(String.concat " "
						(List.map (fun v -> Cs.Vec.get vec v |> Scalar.Rat.to_string)
							!params))
				(Cs.get_c cstr |> Scalar.Rat.to_string)

			let boundary : Boundary.t -> string
				= fun (c,point) ->
				Printf.sprintf "%s\n%s"
					(cstr c)
					(vector point)

			let region : Region.t -> string
				= fun reg ->
                match reg.Region.sx with
                | None -> Pervasives.failwith "unexpected None sx"
				| Some tab -> begin
                    match reg.Region.r with
        			| [] -> Printf.sprintf "%s\n%sno boundaries\n"
        				(vector reg.Region.point)
        				(sx tab)
        			| _ :: _ -> Printf.sprintf "%s\n%sboundaries\n%s\n"
        				(vector reg.Region.point)
        				(sx tab)
        				(String.concat "\n" (List.map (fun (b,_) -> boundary b) reg.Region.r))
                    end

            (** format :
			exp id_region
			cstr
			pointOtherSide

            ou bien

            exp point
             *)
			let explorationPoint : ExplorationPoint.t -> string
				= function
                | ExplorationPoint.Direction (id, (c, pointOtherSide)) ->
                    Printf.sprintf "exp %i\n%s\n%s"
    					id
    					(cstr c)
    					(vector pointOtherSide)
    			| ExplorationPoint.Point point ->
                    Printf.sprintf "exp 0 %s"
                        (vector point)

			let explorationPoints : ExplorationPoint.t list -> string
				= fun points ->
				match points with
				| [] -> "no point"
				| _ :: _ ->  String.concat "\n" (List.map explorationPoint points)
		end

		module Parse = struct

			let paramCoeff : Q.t list -> int -> ParamCoeff.t
				= fun coeffs dimParam ->
				ParamCoeff.mk
					(Misc.sublist coeffs 0 dimParam)
					(List.nth coeffs dimParam)

			let obj : Q.t list list -> int -> int -> Objective.t
				= fun coeffs dimVar dimParam ->
				Objective.mk
					(List.map (fun x -> paramCoeff x dimParam) (Misc.sublist coeffs 0 dimVar))
					(paramCoeff (List.nth coeffs dimVar) dimParam)

			let point : (Q.t * Q.t) list -> Vec.t
				= fun coeffs ->
				List.mapi
					(fun i (a,b) -> let v = Vec.Coeff.add
						(Vec.Coeff.ofQ a)
						(Vec.Coeff.mulr b (Vec.Coeff.delta))
						in
						(v, Naming.to_vpl !naming i))
					coeffs
					|> Vec.mk

			let to_sx : PLPBuild.DescSx.t -> PSplx.t
				= fun d ->
				let rangeVar = Misc.range 0 d.PLPBuild.DescSx.dimVar in
				let rangeParam = Misc.range 0 d.PLPBuild.DescSx.dimParam in
				let obj = obj d.PLPBuild.DescSx.obj d.PLPBuild.DescSx.dimVar d.PLPBuild.DescSx.dimParam in
				let names = List.fold_left
					(fun names i ->
						let v = List.nth d.PLPBuild.DescSx.vars i |> Vec.V.fromInt in
						Naming.allocAt Naming.Var v i names)
					Naming.empty
					rangeVar
				in
				let names = List.fold_left
					(fun names i ->
						let v = List.nth d.PLPBuild.DescSx.params i |> Vec.V.fromInt in
						Naming.allocAt Naming.Param v i names)
					names
					rangeParam
				in
                let sx = {
                    PSplx.obj=obj;
                    PSplx.mat = d.PLPBuild.DescSx.mat;
                    PSplx.basis = d.PLPBuild.DescSx.basis;
                    PSplx.names = names
                } in
                naming := names;
                params := PSplx.getParams sx;
                sx


			let sx : string -> PLPBuild.DescSx.t
				= fun s ->
				try
				PLPParser.one_sx PLPLexer.token (Lexing.from_string s)
				with _ -> begin
					prerr_endline ("parsing error from string\n" ^ s);
				 	failwith "Parsing error"
					end

			let cstr : Q.t list -> Cs.t
				= fun coeffs ->
				let vec = List.map
					(fun i -> (List.nth coeffs i,	Naming.to_vpl !naming i))
					(Misc.range 0 ((List.length coeffs)-1))
				in
				let cst = List.nth coeffs ((List.length coeffs)-1)
				in
				Cs.lt vec cst

			let region : PLPBuild.DescReg.t -> Region.t
				= fun reg ->
				let sx = to_sx reg.PLPBuild.DescReg.sx in
				let pointInside = point reg.PLPBuild.DescReg.point in
				let bnds = List.map
					(fun (cstr_coeffs,point_coeffs) ->
						let cstr = cstr cstr_coeffs in
						let point = point point_coeffs in
						((cstr,point), None))
					reg.PLPBuild.DescReg.bnds
				in
				{
                    Region.sx = Some sx ;
                    Region.point = pointInside ;
                    Region.r = bnds ;
                    Region.id = reg.PLPBuild.DescReg.id
                }

            let explorationPoint_point : (Q.t * Q.t) list -> ExplorationPoint.t
                = fun p ->
                ExplorationPoint.Point (point p)

			let explorationPoint_direction : int * Q.t list * (Q.t * Q.t) list -> ExplorationPoint.t
				= fun (id,c,p)  ->
				let c = cstr c in
			    let p = point p in
				ExplorationPoint.Direction (id,(c,p))

            let explorationPoint : int * Q.t list * (Q.t * Q.t) list -> ExplorationPoint.t
                = fun (id,cstr, point) ->
                if cstr = [] (* point is not a direction !*)
                then explorationPoint_point point
                else explorationPoint_direction (id, cstr, point)

			let slave : PLPBuild.DescSlave.t -> int * Region.t * ExplorationPoint.t list
				= fun slave ->
				let reg = region slave.PLPBuild.DescSlave.reg in
                let explorationPoints = List.map explorationPoint slave.PLPBuild.DescSlave.pointsToExplore in
    				(slave.PLPBuild.DescSlave.remaining_work, reg, explorationPoints)
		end

        module Slave = struct
            let res_slave : t ref = ref empty

        	let print : unit -> unit
        		= fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "current_result :\ntodo = %s"
        			(Misc.list_to_string ExplorationPoint.to_string !res_slave.todo " ; ")))

            let adjust_points : ExplorationPoint.t list -> ExplorationPoint.t list
        		= fun points ->
        		List.fold_left
        			(fun points -> function
                        | ExplorationPoint.Direction (id, point) -> begin
                            try
                                let new_point = Next_Point.RayTracing.adjust Cone (id, point) !res_slave.regs in
                                new_point :: points
                            with Next_Point.RayTracing.Adjacent _ -> points (* TODO: apply adjacency ?*)
                            end
                        | ExplorationPoint.Point p as point ->
                            if MapV.exists (fun _ reg -> Region.contains reg p) !res_slave.regs
                            then points
                            else point :: points)
        			[] points

            module Write = struct

        		let work_to_string : unit -> string
        			= fun () ->
        			Printf.sprintf "todo %i"
        				(List.length !res_slave.todo)

        		(** format :
        		region
        		explorationsPoints
        		remaining_work*)
        		let send : int -> ExplorationPoint.t list -> unit
        			= fun id points ->
        			let reg = MapV.find id !res_slave.regs in
                    let s = Printf.sprintf "problem\nregion %i\n%s%s\n%s\n"
        				id
        				(Print.region reg)
        				(Print.explorationPoints points)
        				(work_to_string())
        			in
                    DebugSlave.log DebugTypes.Normal
                        (lazy (Printf.sprintf "\nSlave %i sending region\n" (getRank())));
                    DebugSlave.log DebugTypes.Detail (lazy s);
        			Mpi.send s 0 0 Mpi.comm_world

        		let ask_for_work : unit -> unit
        			= fun () ->
                    DebugSlave.log DebugTypes.Normal
                        (lazy (Printf.sprintf "Slave %i asking for work" (getRank())));
            		Mpi.send "Done" 0 0 Mpi.comm_world

        	end

            module FirstStep = struct

        		let random_point : unit -> Vec.t
        			= fun ()->
        			List.map (fun v -> Scalar.Rat.mk (Random.int(1000)) (Random.int(1000)+1) |> Vec.Coeff.ofQ
        				, v)
        				!params
        				|> Vec.mk

        		let init : Objective.pivotStrgyT -> PSplx.t -> (ExplorationPoint.t list * local_region_idT) option
        			= fun st sx ->
                    params := PSplx.getParams sx;
                    naming := sx.PSplx.names;
        			let point = random_point () in
        		  	match Exec.exec Cone st sx point with
        		 	| None -> None
        		  	| Some reg -> begin
        		  		(* taken from add_region*)
        		  		let id = get_fresh_id() in
        				let regs = MapV.add id reg MapV.empty in
        				(* XXX:  vérifier les points rendus! (voir addRegion)*)
        				let todo = extract_points reg id in
        				res_slave := {!res_slave with regs = regs};
        				let todo' = adjust_points todo in
                        let len = List.length todo' in
                        res_slave := {!res_slave with todo = Misc.sublist todo' 0 (len/2)};
                        let to_send = Misc.sublist todo' (len/2) len in
        				Some (to_send, id)
        		  	end

        		let run : string -> unit
        			= fun s ->
        			let sx = Parse.sx s
        				|> Parse.to_sx
        			in
        			match init Objective.Bland sx with
        			| None -> ()
        			| Some (points, id) -> Write.send id points
        	end

            module Read = struct

        		let region_already_known : Region.t -> int option
        			= fun reg ->
        			let i = ref 0 in
        			let b = MapV.exists (fun id reg' -> if Region.equal reg reg'
        				then (i := id ; true)
        				else false)
        			(!res_slave.regs)
        			in
        			if b
        			then Some !i
        			else None

        		let replace_id : global_region_idT -> local_region_idT -> unit
        			= fun glob_id loc_id ->
        			if loc_id <> glob_id
        			then begin
        				let map = MapV.add glob_id (MapV.find loc_id !res_slave.regs) !res_slave.regs
        				and todo = List.map
        					(function
                                | ExplorationPoint.Direction (id, p) ->
                                    if id = loc_id
                                    then ExplorationPoint.Direction (glob_id,p)
                                    else ExplorationPoint.Direction (id,p)
                                | ExplorationPoint.Point _ as point -> point)
        					!res_slave.todo
        				in
        				res_slave := {regs = map ; todo = todo}
        			end

        		let parse_problem : string -> unit
        			= fun s ->
        			let (_, reg, points) = PLPParser.problem PLPLexer.token (Lexing.from_string s)
        				|> Parse.slave
        			in
                    let glob_id = reg.Region.id in
        			begin
        			match region_already_known reg with
        			| Some loc_id -> replace_id glob_id loc_id
        			| None -> ()
        			end;
        			let regs = MapV.add glob_id reg !res_slave.regs in
        			let todo = points @ !res_slave.todo in
        			res_slave := {regs = regs ; todo = todo}

        		let parse_points : string -> unit
        			= fun s ->
        			let points = PLPParser.points PLPLexer.token (Lexing.from_string s)
        			|> List.map Parse.explorationPoint
        			in
        			let todo = points @ !res_slave.todo in
        			res_slave := {!res_slave with todo = todo}

        		let run : string -> unit
        			= fun s ->
        			if String.compare (String.sub s 0 7) "problem" = 0
        			then parse_problem s
        			else if String.compare (String.sub s 0 6) "points" = 0
        				then parse_points s
        				else failwith "PLPSlave.Read.run"
        	end

            module Steps = struct

        		let step : unit -> (ExplorationPoint.t list * local_region_idT) option
        			= fun () ->
                    DebugSlave.log DebugTypes.Detail
                        (lazy (Printf.sprintf "Slave %i checking exploration" (getRank())));
        			match !res_slave.todo with
        			| [] -> begin
                        DebugSlave.log DebugTypes.Detail
                            (lazy (Printf.sprintf "Slave %i has nothing to explore" (getRank())));
                        None
                        end
                    | ExplorationPoint.Point _ :: tl -> begin
                        DebugSlave.log DebugTypes.Detail
                            (lazy (Printf.sprintf "Slave %i stops" (getRank())));
                        Pervasives.failwith "PLPDistrib.step: Unexpected point"
                        end
        			| ExplorationPoint.Direction (id_region, (cstr, point)) :: tl ->
                        match (MapV.find id_region !res_slave.regs).Region.sx with
                        | None -> Pervasives.failwith "PLPDistrib.step: unexpected None sx"
                        | Some sx -> begin
                            DebugSlave.log DebugTypes.Detail
                                (lazy (Printf.sprintf "Slave %i launching an exploration" (getRank())));
            				match Exec.exec Cone Objective.Bland sx point with
            			 	| None -> None
            			  	| Some reg -> begin
                                res_slave := {!res_slave with todo = List.tl !res_slave.todo};
                                if Read.region_already_known reg <> None
                                then None
                                else begin
                			  		(* taken from add_region*)
                			  		let id = get_fresh_id() in
                					let regs = MapV.add id reg !res_slave.regs in
                					res_slave := {!res_slave with regs = regs};
                					let points = [ExplorationPoint.Direction (id_region, (cstr, point))] @ (extract_points reg id) in
                                    let todo = adjust_points points in
                                    let len = List.length todo in
                                    res_slave := {!res_slave with todo = todo @ (Misc.sublist todo 0 (len/2))};
                                    let to_send = Misc.sublist todo (len/2) len in
                					Some (to_send, id)
                                end
            			  	end
                        end

        		let run : unit -> unit
        			= fun () ->
        			match step() with
        			| None -> ()
        			| Some (points, loc_id) -> Write.send loc_id points

        	end

            let read_wait: unit -> string
                = fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i waiting for work" (getRank())));
                let s = Mpi.(receive 0 any_tag comm_world) in
                DebugSlave.log DebugTypes.Detail
                    (lazy (Printf.sprintf "Slave %i received string\n%s" (getRank()) s));
                s

            (** If there is something to read, read it. Otherwise, returns None. *)
            let read : unit -> string option
            	= fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i checking mails" (getRank())));
                let res = match Mpi.(iprobe 0 any_tag comm_world) with
                | None -> None
                | Some (_,_) -> Some (read_wait ())
                in
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i done checking mails" (getRank())));
                res

            let rec more_whip : unit -> unit
            	= fun () ->
            	print() ;
            	match !res_slave.todo with
            	| [] -> begin (* Plus de point à traiter. *)
            		Write.ask_for_work();
            		let s = read_wait() in
            		Read.run s;
            		more_whip()
            		end
            	| _ :: _ -> begin
            		match read() with
            		| None -> begin (* Rien à lire *)
            			Steps.run();
            			more_whip()
            			end
            		| Some s -> begin
            			Read.run s;
            			more_whip()
            			end
            		end

            let first_whip: unit -> unit
            	= fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i rises" (getRank())));
            	let s = read_wait() in
            	FirstStep.run s;
            	more_whip()

        end



		module MapId = Map.Make(struct type t = int let compare = Pervasives.compare end)

		(** Associates a local region id to a global region id (key). *)
		type map_process_regionT = local_region_idT MapId.t

		(** Associates a map_process_regionT to a process id(key). *)
		type mapIdT = map_process_regionT MapId.t

		(** Associates to a processus id the number of points it has left to explore. *)
		type mapWorkT = int MapId.t

		type t = {
			regs : mapRegs_t; (** associates global region id to regions. *)
			mapId : mapIdT;
			mapWork : mapWorkT;
			todo : ExplorationPoint.t list;
		}

		let empty = {
			regs = MapV.empty;
			mapId = MapId.empty;
			mapWork = MapId.empty;
			todo = []
		}

		(*
		let to_string : unit () -> string
			= fun () ->
			Printf.sprintf "Current result : \n\t%s\n\t%s\n\t%s\n\t%s"
			()
		*)
		let result : t ref = ref empty

		let set_work : Mpi.rank -> int -> unit
			= fun pr_id work ->
			result := {!result with mapWork = MapId.add pr_id work !result.mapWork}

		let get_work : Mpi.rank -> int
			= fun pr_id ->
			try MapId.find pr_id !result.mapWork
			with Not_found -> 0

		let add_work : Mpi.rank -> int -> unit
			= fun pr_id work ->
			let prev_work = get_work pr_id in
			result := {!result with mapWork = MapId.add pr_id (work + prev_work) !result.mapWork}

		let mapWork_to_string : unit -> string
			= fun () ->
			Misc.list_to_string
				(fun (process_id,work) -> Printf.sprintf "(%i -> %i)" process_id work)
				(MapId.bindings !result.mapWork) " ; "

		(** Returns the local id of the region identified globally as [glob_id] in processus [pr_id]. *)
		let get_region_id : Mpi.rank -> global_region_idT -> local_region_idT
			= fun pr_id glob_id ->
			try
				let map = MapId.find pr_id !result.mapId in
				try
					MapId.find glob_id map
				with Not_found -> Pervasives.failwith "PLP.get_id : region not found"
			with Not_found -> Pervasives.failwith "PLP.get_id : process not found"

		(** Set the local id of the region identified globally as [glob_id] in processus [pr_id] as [loc_id]. *)
		let set_region_id : Mpi.rank -> global_region_idT -> local_region_idT -> unit
			= fun pr_id glob_id loc_id ->
			let map = MapId.add glob_id loc_id
				(try MapId.find pr_id !result.mapId
				with Not_found -> MapId.empty)
			in
			result := {!result with mapId = MapId.add pr_id map !result.mapId}

        let get_slave_ranks : unit -> Mpi.rank list
            = fun () ->
            Misc.range 1 (Mpi.comm_size Mpi.comm_world)

		let region_already_known : Region.t -> bool
			= fun reg ->
			MapV.exists (fun _ reg' -> Region.equal reg reg') (!result.regs)

        (** Returns a fresh id for a new region.
 		Ids given by master are negative. *)
 		let get_fresh_id : unit -> int
            = fun () ->
 			let id = !reg_id in
 			reg_id := id - 1;
 			id

		let add_region : Mpi.rank -> (int * Region.t * ExplorationPoint.t list)
			-> (global_region_idT * ExplorationPoint.t list) option
			(* explorationPoints talk about the new region. Put the global id instead of the local one. *)
			= let update_explorationPoints : global_region_idT -> ExplorationPoint.t list -> ExplorationPoint.t list
				= fun glob_id points ->
				List.map
                    (function
                        | ExplorationPoint.Direction (_,p) -> ExplorationPoint.Direction (glob_id, p)
                        | p -> p)
                    points
			in
			fun sender_process_id (remaining_work, reg, explorationPoints) ->
            set_work sender_process_id remaining_work;
			let glob_id = get_fresh_id() in
            let loc_id = reg.Region.id in
			if region_already_known reg
			then begin
				DebugMaster.log DebugTypes.Normal (lazy "Region already known");
				None
			end
			else begin
				result := {!result with regs = MapV.add glob_id reg !result.regs};
				(* Setting region ids*)
				List.iter
					(fun i ->
						if i = sender_process_id
						then set_region_id i glob_id loc_id
						else set_region_id i glob_id glob_id)
					(get_slave_ranks());
				Some (glob_id, update_explorationPoints glob_id explorationPoints)
			end

		let set_explorationPoint : Mpi.rank -> ExplorationPoint.t -> ExplorationPoint.t
			= fun process_id -> function
            | ExplorationPoint.Direction (glob_id, p) ->
                ExplorationPoint.Direction (get_region_id process_id glob_id, p)
            | p -> p

		(** Gives a fraction of todo to the process. *)
		let giveWork : Mpi.rank -> unit
			= fun pr_id ->
			if get_work pr_id = 0 && !result.todo <> []
			then begin
                let len_todo = List.length !result.todo in
                let n_slaves = Mpi.comm_size Mpi.comm_world - 1 in
                let ratio = max 1 (len_todo/n_slaves) in
                let work = Misc.sublist !result.todo 0 ratio in
                result := {!result with todo = Misc.sublist !result.todo ratio len_todo};
                set_work pr_id ratio;
                (*
                let msg = List.map
                    (function
                    | ExplorationPoint.Point _ as point -> Printf.sprintf "points\n%s\n"
                        (Print.explorationPoint point)
                    | ExplorationPoint.Direction (glob_id, p) -> Printf.sprintf "points\n%s\n"
                        (set_explorationPoint pr_id (ExplorationPoint.Direction (glob_id, p))
                        |> Print.explorationPoint)
                    ) work
                |> String.concat "\n"
                in
                Mpi.send msg pr_id 0 Mpi.comm_world*)
                List.iter
                    (fun work -> let msg = match work with
                        | ExplorationPoint.Point _ as point -> Printf.sprintf "points\n%s\n"
                            (Print.explorationPoint point)
                        | ExplorationPoint.Direction (glob_id, p) -> Printf.sprintf "points\n%s\n"
                            (set_explorationPoint pr_id (ExplorationPoint.Direction (glob_id, p))
                            |> Print.explorationPoint)
                    in
                    Mpi.send msg pr_id 0 Mpi.comm_world
                    )
                    work
                (*
                let work = List.hd !result.todo in
                result := {!result with todo = List.tl !result.todo};
                set_work pr_id 1;
                let msg = match work with
                    | ExplorationPoint.Point _ as point -> Printf.sprintf "points\n%s\n"
                        (Print.explorationPoint point)
                    | ExplorationPoint.Direction (glob_id, p) -> Printf.sprintf "points\n%s\n"
                        (set_explorationPoint pr_id (ExplorationPoint.Direction (glob_id, p))
                        |> Print.explorationPoint)
                in
                Mpi.send msg pr_id 0 Mpi.comm_world
                *)
			end


		let saveWork : global_region_idT -> ExplorationPoint.t list -> unit
			= fun glob_id points ->
			result := {!result with
				todo = !result.todo @
				(List.map
					(function
                        | ExplorationPoint.Direction (loc_id, p) -> ExplorationPoint.Direction (glob_id, p)
                        | p -> p)
					points)
			}

		(** Propagate the new region (glob_id) to every process, except the sender. *)
		let propagateRegion : global_region_idT -> Mpi.rank -> unit
			= fun glob_id sender_process ->
			let reg = MapV.find glob_id !result.regs in
			let str : Mpi.rank -> string
				= fun pr_id ->
				let loc_id = get_region_id pr_id glob_id in
				Printf.sprintf "problem\nregion %i\n%sno point\ntodo 0\n" (* 0 est le nombre de travail restant (inutile dans cette communication) *)
				loc_id
				(Print.region reg)
			in
			List.iter
                (fun process_id' ->
    				if process_id' <> sender_process
    				then Mpi.send (str process_id') process_id' 0 Mpi.comm_world)
				(get_slave_ranks())

		let init : PSplx.t -> unit
			= fun sx ->
			let msg = Print.sx sx in
			DebugMaster.log DebugTypes.Normal
				(lazy (Printf.sprintf "Initializing with string\n%s" msg));
			(*Mpi.broadcast s 0 Mpi.comm_world;*)
            List.iter
                (fun rank -> Mpi.send msg rank 0 Mpi.comm_world)
				(get_slave_ranks())

		let stop : unit -> bool
			= fun () ->
			print_endline (mapWork_to_string());
            Printf.sprintf "Work left to do : %i" (List.length !result.todo)
                |> print_endline;
			!result.todo = []
			&&	(MapId.for_all (fun _ work -> work = 0) !result.mapWork)

		let distrib_work : unit -> unit
			= fun () ->
			List.iter giveWork (get_slave_ranks ())

		let asking_for_work : Mpi.rank -> unit
			= fun process_id ->
			set_work process_id 0

        type resT =
            | AskForWork of Mpi.rank
            | NewRegion of Mpi.rank * (int * Region.t * ExplorationPoint.t list)
                (* (node_rank, (remaining_work, region, points)) *)

        let treat_string  : string -> Mpi.rank -> resT
            = fun s process_id ->
            if String.compare s "Done" = 0
            then begin
                DebugMaster.log DebugTypes.Detail
                    (lazy (Printf.sprintf "process %i is asking for work" process_id));
                AskForWork process_id
            end
            else begin
                DebugMaster.log DebugTypes.Detail
                    (lazy (Printf.sprintf "received from process %i region :\n%s" process_id s));
                NewRegion
                    (process_id,
                     PLPParser.problem PLPLexer.token (Lexing.from_string s)
                        |> Parse.slave)
            end

        let rec wait_res : unit -> resT
            = fun () ->
            let (s, id, _) = Mpi.(receive_status any_source any_tag comm_world) in
            treat_string s id

		(* must be done at least once after initialization *)
		let rec steps : (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
			= fun get_cert ->
			DebugMaster.log DebugTypes.Normal (lazy "Steps");
			begin
				match wait_res() with
				| NewRegion (process_id, slave_result) -> begin
					match add_region process_id slave_result with
					| None -> ()
					| Some (glob_id, points) -> begin
						propagateRegion glob_id process_id;
						saveWork glob_id points;
						distrib_work()
						end
					end
				| AskForWork process_id -> begin
					asking_for_work process_id;
					giveWork process_id
					end
			end;
			if stop()
			then get_results {regs = !result.regs ; todo = []} get_cert
			else steps get_cert

		let init_state : unit -> unit
			= fun () ->
			reg_id := -1;
			result := empty

		let run : Objective.pivotStrgyT -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
			= fun st sx get_cert ->
			init_state();
            if Mpi.comm_rank Mpi.comm_world = 0
            then
    			let point = Vec.nil in
    			match InitSx.init_sx st sx point with
    			| None -> None
    			| Some sx -> begin
    				init sx
    			end;
                steps get_cert
            else Pervasives.failwith "I'm slave, no master!"

	end

    let run : config -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
        = fun config sx get_cert ->
        if Mpi.comm_size Mpi.comm_world = 1
        then run config sx get_cert
        else Distributed.run config.stgy sx get_cert
end
