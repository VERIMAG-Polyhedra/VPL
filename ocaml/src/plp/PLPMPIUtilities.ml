module Cs = PLPCore.Cs
module EqSet = PLPCore.EqSet
module Cons = PLPCore.Cons

module DebugMaster = DebugTypes.Debug(struct let name = "PLP_Master" end)
module DebugSlave = DebugTypes.Debug(struct let name = "PLP_Slave" end)

module Lift(Minimization : Min.Type) = struct

	include PLPPlot.Plot(Minimization)

    (* Common utilities *)
    module Utilities = struct

        (** index of the process in the list of processes.*)
        type global_region_idT = int
        type local_region_idT = int

        module MapId = Map.Make(struct type t = int let compare = Pervasives.compare end)

        let map_to_string : Region.t MapV.t -> string
            = fun map ->
            Printf.sprintf "map:\n %s"
                (Misc.list_to_string
                    (fun (id,_) -> string_of_int id)
                    (MapV.bindings map)
                    " ; ")

        (* Parameters of the problem. *)
        (* TODO : ne pas oublier de l'initialiser !*)
        let params : Vec.V.t list ref = ref []
        let naming : Naming.t ref = ref Naming.empty

        let getRank : unit -> Mpi.rank
            = fun () ->
            Mpi.comm_rank Mpi.comm_world

        let isMaster : unit -> bool
            = fun () ->
            getRank () = 0

        let get_slave_ranks : unit -> Mpi.rank list
            = fun () ->
            Misc.range 1 (Mpi.comm_size Mpi.comm_world)

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
                let s = match reg.Region.sx with
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
                in
                Printf.sprintf "region %i\n%s" reg.Region.id s

            let regions : Region.t list -> string
                = fun l ->
                List.map region l
                |> String.concat "\n"

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
    				(fun i -> (List.nth coeffs i, Naming.to_vpl !naming i))
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
    			= fun (id, c, p) ->
    			let c = cstr c in
    		    let p = point p in
    			ExplorationPoint.Direction (id,(c,p))

            let explorationPoint : int * Q.t list * (Q.t * Q.t) list -> ExplorationPoint.t
                = fun (id, cstr, point) ->
                if cstr = [] (* point is not a direction !*)
                then explorationPoint_point point
                else explorationPoint_direction (id, cstr, point)

    		let slave : PLPBuild.DescSlave.t -> int * (Region.t * ExplorationPoint.t list) list
    			= fun slave ->
                let regsAndPoints = List.map
                    (fun (reg,points) ->
                        region reg,
                        List.map explorationPoint points)
                    slave.PLPBuild.DescSlave.regsAndPoints
                in
                (slave.PLPBuild.DescSlave.remaining_work, regsAndPoints)
    	end

        (* Utilities reserved for the Master *)
        module MasterUtilities = struct

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

            let result : t ref = ref empty

            type resT =
                | AskForWork of Mpi.rank
                | NewRegions of Mpi.rank * (int * (Region.t * ExplorationPoint.t list) list)
                    (* (node_rank, (remaining_work, region, points)) *)

            let get_region : int -> Region.t
                = fun reg_id ->
                MapV.find reg_id !result.regs

            let region_already_known : Region.t -> bool
    			= fun reg ->
    			MapV.exists (fun _ reg' -> Region.equal reg reg') (!result.regs)

            (** Set the local id of the region identified globally as [glob_id] in processus [pr_id] as [loc_id]. *)
    		let set_region_id : Mpi.rank -> global_region_idT -> local_region_idT -> unit
    			= fun pr_id glob_id loc_id ->
    			let map = MapId.add glob_id loc_id
    				(try MapId.find pr_id !result.mapId
    				with Not_found -> MapId.empty)
    			in
    			result := {!result with mapId = MapId.add pr_id map !result.mapId}

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
                (*set_work sender_process_id remaining_work;*)
    			let glob_id = get_fresh_id() in
                let loc_id = reg.Region.id in
    			if region_already_known reg
    			then begin
    				DebugMaster.log DebugTypes.Normal (lazy "Region already known");
    				None
    			end
    			else begin
                    DebugMaster.log DebugTypes.Normal (lazy "Region unknown");
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

        	(** Returns the local id of the region identified globally as [glob_id] in processus [pr_id]. *)
    		let get_region_id : Mpi.rank -> global_region_idT -> local_region_idT
    			= fun pr_id glob_id ->
    			try
    				let map = MapId.find pr_id !result.mapId in
    				try
    					MapId.find glob_id map
    				with Not_found -> Pervasives.failwith "PLP.get_id : region not found"
    			with Not_found -> Pervasives.failwith "PLP.get_id : process not found"

            let set_explorationPoint : Mpi.rank -> ExplorationPoint.t -> ExplorationPoint.t
    			= fun process_id -> function
                | ExplorationPoint.Direction (glob_id, p) ->
                    ExplorationPoint.Direction (get_region_id process_id glob_id, p)
                | p -> p
        end (* End Master *)

        module SlaveUtilities = struct

            let res_slave : t ref = ref empty

            let get_region : int -> Region.t
                = fun reg_id ->
                try MapV.find reg_id !res_slave.regs
                with _ -> begin
                    lazy (Printf.sprintf "Region %i not found in map %s"
                        reg_id
                        (map_to_string !res_slave.regs))
                        |> DebugSlave.log DebugTypes.Normal;
                    raise Not_found
                end
        end (* End Slave *)

        module type MPIMethod = sig
            module Slave : sig
                val workRequest : unit -> unit
                val chooseResultsToSend : ExplorationPoint.t list -> int list * ExplorationPoint.t list (* int list : indices of regions to send *)
            end

            module Master : sig
                val workSend : unit -> unit
                (* [gatherResults msg proces_id] *)
                val gatherResults : string -> int -> unit
            end
        end
    end
end
