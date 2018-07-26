module Cs = PLPCore.Cs
module EqSet = PLPCore.EqSet
module Cons = PLPCore.Cons

module DebugMaster = PLPMPIUtilities.DebugMaster
module DebugSlave = PLPMPIUtilities.DebugSlave

module PLP(Minimization : Min.Type) = struct

	include PLPMPIFunctor.Lift(Minimization)

    module MethodLegacy : Utilities.MPIMethod = struct
        open Utilities

        module Slave = struct

            open SlaveUtilities
            let workRequest	= fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i asking for work" (getRank())));
        		Mpi.send "Done" 0 0 Mpi.comm_world

            let chooseResultsToSend = fun todo ->
                let len = List.length todo in
                res_slave := {!res_slave with todo = todo @ (Misc.sublist todo 0 (len/2))};
                let to_send = Misc.sublist todo (len/2) len in
                ([],to_send)

        end

        module Master = struct
            open MasterUtilities

            let workSend () = ()

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
                    NewRegions
                        (process_id,
                         PLPParser.problem PLPLexer.token (Lexing.from_string s)
                            |> Parse.slave)
                end

            (** Propagate the new region (glob_id) to every process, except the sender. *)
    		let propagateRegion : global_region_idT -> Mpi.rank -> unit
    			= fun glob_id sender_process ->
                lazy (Printf.sprintf "Propagating region %i" glob_id)
                    |> DebugMaster.log DebugTypes.Normal;
    			let str : Mpi.rank -> string
    				= fun pr_id ->
                    lazy (Printf.sprintf "Propagating region %i to process %i" glob_id pr_id)
                        |> DebugMaster.log DebugTypes.Detail;
                    let loc_id = get_region_id pr_id glob_id in
    				Printf.sprintf "problem\nregion %i\n%sno point\ntodo 0\n" (* 0 est le nombre de travail restant (inutile dans cette communication) *)
    				loc_id
    				(Print.region (get_region glob_id))
    			in
    			List.iter
                    (fun process_id' ->
        				if process_id' <> sender_process
        				then Mpi.send (str process_id') process_id' 0 Mpi.comm_world)
    				(get_slave_ranks())

        	let saveWork : global_region_idT -> ExplorationPoint.t list -> unit
    			= fun glob_id points ->
                lazy "Saving work"
                    |> DebugMaster.log DebugTypes.Normal;
    			result := {!result with
    				todo = !result.todo @
    				(List.map
    					(function
                            | ExplorationPoint.Direction (loc_id, p) ->                        ExplorationPoint.Direction (glob_id, p)
                            | p -> p)
    					points)
    			}

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
                    List.iter
                        (fun work -> let msg = match work with
                            | ExplorationPoint.Point _ as point -> Printf.sprintf "points\n%s\n"
                                (Print.explorationPoint point)
                            | ExplorationPoint.Direction (glob_id, p) -> Printf.sprintf "points\n%s\n"
                                (set_explorationPoint pr_id (ExplorationPoint.Direction (glob_id, p))
                                |> Print.explorationPoint)
                        in
                        lazy (Printf.sprintf "Sending to slave %i point %s" pr_id msg)
                            |> DebugMaster.log DebugTypes.Detail;
                        Mpi.send msg pr_id 0 Mpi.comm_world
                        )
                        work
    			end

            let broadcastWork : unit -> unit
    			= fun () ->
                lazy "Broadcasting work"
                    |> DebugMaster.log DebugTypes.Normal;
    			List.iter giveWork (get_slave_ranks ())

            let asking_for_work : Mpi.rank -> unit
    			= fun process_id ->
    			set_work process_id 0

            let gatherResults = fun msg process_id ->
            	match treat_string msg process_id with
				| NewRegions (process_id, (remainingwork, regsAndPoints)) -> begin
                    DebugMaster.log DebugTypes.Normal (lazy "NewRegions parsed");
                    List.iter
                        (fun (reg, points) ->
    					match add_region process_id (remainingwork, reg, points) with
    					| None -> ()
    					| Some (glob_id, points) -> begin
                            lazy (Printf.sprintf "Region %i added" glob_id)
                                |> DebugMaster.log DebugTypes.Normal;
    						propagateRegion glob_id process_id;
    						saveWork glob_id points;
    						broadcastWork ()
    						end
                        ) regsAndPoints
					end
				| AskForWork process_id -> begin
                    DebugMaster.log DebugTypes.Normal (lazy "AskForWork parsed");
					asking_for_work process_id;
					giveWork process_id
					end

        end
    end (* End MethodLegacy *)


    module MethodMasterSlaveBasic = struct
        open Utilities

        module Slave = struct
            open SlaveUtilities

            let workRequest : unit -> unit
                = fun () ->
                DebugSlave.log DebugTypes.Normal
                    (lazy (Printf.sprintf "Slave %i asking for work" (getRank())));
        		Mpi.send "Done" 0 0 Mpi.comm_world

            (*
            val chooseResultsToSend : ExplorationPoint.t list -> Region.t list * ExplorationPoint.t list
                = fun () *)
        end
        (*
        module Master : sig
            val workSend : unit -> unit
            (* [gatherResults msg proces_id] *)
            val gatherResults : string -> int -> unit
        end
        *)
    end (* End MethodMasterSlaveBasic *)

    module MPILegacy = MPI (MethodLegacy)

    let run : config -> PSplx.t -> (PSplx.t -> 'c) -> (Region.t * 'c Cons.t) list option
        = fun config sx get_cert ->
        if Mpi.comm_size Mpi.comm_world = 1
        then run config sx get_cert
        else MPILegacy.Master.run config.stgy sx get_cert
end
