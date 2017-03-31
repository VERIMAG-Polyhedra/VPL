open DebugTypes

type t = Min | MinLP | Proj | PLPCore | Pol | PSplx | Horacle | Join

let modules : t list = [Min ; MinLP ;Proj ; PLPCore ; Pol ; PSplx ; Horacle ; Join]

let print_enable : unit -> unit
	= fun () ->
	List.iter
		(function 
		| Min -> Min.Debug.print_enable()
		| MinLP -> MinLP.Debug.print_enable()
		| Proj -> Proj.Debug.print_enable()
		| PLPCore -> PLPCore.Debug.print_enable()
		| Pol -> Pol.Debug.print_enable()
		| PSplx -> PSplx.Debug.print_enable()
		| Horacle -> ()(*failwith "RESTAURE cas Horacle de Debug.ml"*)
                  (* A RESTAURER: ligne ci-dessous *)
                  (* HOtypes.Debug.print_enable() *)
		| Join -> Join.Debug.print_enable())
		modules

let print_disable : unit -> unit
 = fun () ->
	List.iter
		(function 
		| Min -> Min.Debug.print_disable()
		| MinLP -> MinLP.Debug.print_disable()
		| Proj -> Proj.Debug.print_disable()
		| PLPCore -> PLPCore.Debug.print_disable()
		| Pol -> Pol.Debug.print_disable()
		| PSplx -> PSplx.Debug.print_disable()
		| Join -> Join.Debug.print_disable()
		| Horacle -> ()(*failwith "RESTAURE cas Horacle de Debug.ml"*)
                  (* A RESTAURER: ligne ci-dessous *)
                  (* HOtypes.Debug.print_disable() *)
                )
		modules
  
let level_selection : t -> levelT list
	= function
	| Min -> [Title ; MInput ; MOutput]
	| MinLP -> []
	| Proj -> [] 
	| PLPCore -> [Title ; MInput ; MOutput ; Normal ; Detail]
	| Pol -> [Title ; MInput ; MOutput]
	| PSplx -> []
	| Horacle -> [Title ; MInput ; MOutput ]
	| Join -> [Title ; MInput ; MOutput]
	
let colors : t -> color 
	= function
	| Min -> Cyan
	| MinLP -> Green
	| Proj -> Magenta 
	| PLPCore -> Red
	| Pol -> Blue
	| PSplx -> Yellow
	| Horacle -> White
	| Join -> Magenta
	
let set_colors : unit -> unit
	= fun () ->
	List.iter
		(fun m -> let color = colors m in
		match m with 
		| Min -> Min.Debug.set_color color
		| MinLP -> MinLP.Debug.set_color color
		| Proj -> Proj.Debug.set_color color
		| PLPCore -> PLPCore.Debug.set_color color
		| Pol -> Pol.Debug.set_color color
		| PSplx -> PSplx.Debug.set_color color
		| Horacle -> 
                  ()(*failwith "RESTAURE cas Horacle de Debug.ml"*)
                  (* A RESTAURER: ligne ci-dessous *)
                  (* HOtypes.Debug.set_color color *)
		| Join -> Join.Debug.set_color color)
	modules
	
let enable_one : t -> levelT list -> unit
	= fun m lvls ->
	match m with
	| Min -> Min.Debug.enable lvls
	| MinLP -> MinLP.Debug.enable lvls
	| Proj -> Proj.Debug.enable lvls
	| PLPCore -> PLPCore.Debug.enable lvls
	| Pol -> Pol.Debug.enable lvls
	| PSplx -> PSplx.Debug.enable lvls
	| Horacle ->
                  ()(*failwith "RESTAURE cas Horacle de Debug.ml"*)
                  (* A RESTAURER: ligne ci-dessous *)
                  (* HOtypes.Debug.enable lvls *)
	| Join -> Join.Debug.enable lvls
	
let enable : unit -> unit
	= fun () ->
	List.iter
		(fun m -> enable_one m (level_selection m))
	modules 
	  
let disable : unit -> unit
 = fun () -> 
 	List.iter
		(function 
		| Min -> Min.Debug.disable()
		| MinLP -> MinLP.Debug.disable()
		| Proj -> Proj.Debug.disable()
		| PLPCore -> PLPCore.Debug.disable()
		| Pol -> Pol.Debug.disable()
		| PSplx -> PSplx.Debug.disable()
		| Horacle ->
                  ()(*failwith "RESTAURE cas Horacle de Debug.ml"*)
                  (* A RESTAURER: ligne ci-dessous *)
                  (* HOtypes.Debug.disable() *)
		| Join -> Join.Debug.disable())
	modules

	
let get_trace : unit -> string list
  = fun () -> List.rev !DebugTypes.trace

let reset_trace : unit -> unit
  = fun () -> DebugTypes.trace := []
