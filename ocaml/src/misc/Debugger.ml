open DebugTypes

let modules : ((module Type) * color) list = [
    (module Min.Debug), Cyan;
    (module Join.Debug), Magenta;
    (module Proj.Debug), Magenta;
    (module PLPCore.Debug), Red;
    (module MinLP.Debug), Green;
    (module HOtypes.Debug), White;
	(module OraclePattern.Debug), White;
    (module Pol.Debug), Blue;
    (module PSplx.Debug), Yellow;
]

let print_enable : unit -> unit
	= fun () ->
	List.iter (fun (m,_) ->
        let module M = (val m : Type) in
        M.print_enable())
        modules

let print_disable : unit -> unit
	= fun () ->
	List.iter (fun (m,_) ->
        let module M = (val m : Type) in
        M.print_disable())
        modules

let set_colors : unit -> unit
	= fun () ->
	List.iter (fun (m,color) ->
        let module M = (val m : Type) in
        M.set_color color) modules

let enable : unit -> unit
	= fun () ->
	List.iter (fun (m,_) ->
        let module M = (val m : Type) in
        M.enable DebugTypes.([Title ; MInput ; MOutput]))
        modules

let disable : unit -> unit
 = fun () ->
 	List.iter (fun (m,_) ->
        let module M = (val m : Type) in
        M.disable())
        modules

let get_trace : unit -> string list
  = fun () -> List.rev !DebugTypes.trace

let reset_trace : unit -> unit
  = fun () -> DebugTypes.trace := []
