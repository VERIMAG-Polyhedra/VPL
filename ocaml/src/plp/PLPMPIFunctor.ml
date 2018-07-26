module Cs = PLPCore.Cs
module EqSet = PLPCore.EqSet
module Cons = PLPCore.Cons

module DebugMaster = PLPMPIUtilities.DebugMaster
module DebugSlave = PLPMPIUtilities.DebugSlave

module Lift(Minimization : Min.Type) = struct

    include PLPMPIUtilities.Lift(Minimization)

	module MPI (Method: Utilities.MPIMethod) = struct

        open Utilities

        module Slave = struct

            open SlaveUtilities

            let already_asked_for_work : bool ref = ref false

        	let print : unit -> unit
        		= fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "current_result :\n%s\ntodo = %s"
                    (map_to_string !res_slave.regs)
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
        		let send : Region.t list -> ExplorationPoint.t list -> unit
        			= fun regs points ->
                    let s = Printf.sprintf "problem\n%s%s\n%s\n"
        				(Print.regions regs)
        				(Print.explorationPoints points)
        				(work_to_string())
        			in
                    if !res_slave.todo = []
                    then already_asked_for_work := true;
                    DebugSlave.log DebugTypes.Normal
                        (lazy (Printf.sprintf "\nSlave %i sending region\n" (getRank())));
                    DebugSlave.log DebugTypes.Detail (lazy s);
        			Mpi.send s 0 0 Mpi.comm_world
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
        		  		let id = reg.Region.id in
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
        			| Some (points, id) ->
                        let reg = get_region id in
                        Write.send [reg] points
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
        			let (_, regsAndPoints) = PLPParser.problem PLPLexer.token (Lexing.from_string s)
        				|> Parse.slave
        			in
                    List.iter
                        (fun (reg,points) ->
                            let glob_id = reg.Region.id in
                			begin
                			match region_already_known reg with
                			| Some loc_id -> replace_id glob_id loc_id
                			| None -> ()
                			end;
                			let regs = MapV.add glob_id reg !res_slave.regs in
                			let todo = points @ !res_slave.todo in
                			res_slave := {regs = regs ; todo = todo}
                        ) regsAndPoints

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

        		let step : unit -> ((int list * ExplorationPoint.t list) * local_region_idT) option
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
                        match (get_region id_region).Region.sx with
                        | None -> Pervasives.failwith "PLPDistrib.step: unexpected None sx"
                        | Some sx -> begin
                            DebugSlave.log DebugTypes.Detail
                                (lazy (Printf.sprintf "Slave %i launching an exploration" (getRank())));
            				match Exec.exec Cone Objective.Bland sx point with
            			 	| None -> None
            			  	| Some reg -> begin
                                DebugSlave.log DebugTypes.Detail
                                    (lazy (Printf.sprintf "Slave %i ending an exploration" (getRank())));
                                res_slave := {!res_slave with todo = List.tl !res_slave.todo};
                                if Read.region_already_known reg <> None
                                then begin
                                    DebugSlave.log DebugTypes.Detail
                                        (lazy (Printf.sprintf "Region already known by slave %i" (getRank())));
                                        None
                                end
                                else begin
                			  		(* taken from add_region *)
                                    DebugSlave.log DebugTypes.Detail
                                        (lazy (Printf.sprintf "Slave %i found new region" (getRank())));
                			  		let id = reg.Region.id in
                					let regs = MapV.add id reg !res_slave.regs in
                					res_slave := {!res_slave with regs = regs};
                					let points = ExplorationPoint.Direction (id_region, (cstr, point)) :: (extract_points reg id) in
                                    let todo = adjust_points points in
                                    let to_send = Method.Slave.chooseResultsToSend todo in
                                    Some (to_send, id)
                                end
            			  	end
                        end

        		let run : unit -> unit
        			= fun () ->
        			match step() with
        			| None -> ()
        			| Some ((ids_reg,points), loc_id) ->
                        let regs = List.map get_region (loc_id :: ids_reg) in
                        Write.send regs points

        	end

            let read_wait: unit -> string
                = fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i waiting for work" (getRank())));
                let s = Mpi.(receive 0 any_tag comm_world) in
                DebugSlave.log DebugTypes.Detail
                    (lazy (Printf.sprintf "Slave %i received string\n%s" (getRank()) s));
                s

            let first_read: unit -> string
                = fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i waiting for simplex tableau" (getRank())));
                let s = Mpi.(broadcast "" 0 comm_world) in
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
                    (*if not !already_asked_for_work
                	then *)Method.Slave.workRequest();
            		let s = read_wait() in
                    already_asked_for_work := false;
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
            	let s = first_read() in
            	FirstStep.run s;
            	more_whip()

        end (* End slave *)

        module Master = struct

            open MasterUtilities

    		let mapWork_to_string : unit -> string
    			= fun () ->
    			Misc.list_to_string
    				(fun (process_id,work) -> Printf.sprintf "(%i -> %i)" process_id work)
    				(MapId.bindings !result.mapWork) " ; "

    		let init : PSplx.t -> unit
    			= fun sx ->
    			let msg = Print.sx sx in
    			DebugMaster.log DebugTypes.Normal
    				(lazy (Printf.sprintf "Initializing with string\n%s" msg));
    			let _ = Mpi.broadcast msg 0 Mpi.comm_world in
                ()

    		let stop : unit -> bool
    			= fun () ->
    			print_endline (mapWork_to_string());
                Printf.sprintf "Work left to do : %i" (List.length !result.todo)
                    |> print_endline;
    			!result.todo = []
    			&&	(MapId.for_all (fun _ work -> work = 0) !result.mapWork)

            let rec wait_res : unit -> unit
                = fun () ->
                let (s, id, _) = Mpi.(receive_status any_source any_tag comm_world) in
                Method.Master.gatherResults s id

    		let rec steps : (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
    			= fun get_cert ->
    			DebugMaster.log DebugTypes.Normal (lazy "Steps");
    			wait_res ();
                Method.Master.workSend ();
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
                        naming := sx.PSplx.names;
                        params := PSplx.getParams sx;
        				init sx
        			end;
                    steps get_cert
                else Pervasives.failwith "I'm slave, no master!"
        end (* End Master *)
	end (* End functor MPI *)
end
