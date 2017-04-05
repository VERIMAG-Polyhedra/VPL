module MapPro = Map.Make(struct type t = string let compare = Pervasives.compare end)

type element = {
	start : Z.t option;
	time : Z.t;
}

let e0 = {
	start = None;
	time = Z.zero;
}

let map : element MapPro.t ref = ref MapPro.empty

exception Already_started of string
exception Not_started of string

module type Type = sig
	module D : sig val name : string end
	
	(** Reset the whole map. *)
	val reset : unit -> unit
	
	(** Starts the timer for the given function name.
		@raise Already_started if that timer was already launched. *)
	val start : string -> unit
end

module Profile (D : sig val name : string end) = struct
	module D = D
	
	let reset : unit -> unit
		= fun () ->
		map := MapPro.empty
	
	let get_name : string -> string
		= fun s ->
		String.concat "." [D.name ; s]
	
	let toZ : int64 -> Z.t
		= fun time ->
		Z.of_int64 time
	
	let start : string -> unit
		= fun name ->
		let fullname = get_name name in
		try 
			let e = MapPro.find fullname !map in
			if e.start = None
			then let start_time =  Oclock.gettime Oclock.realtime |> toZ in
				map := MapPro.add fullname {e with start = Some start_time} !map
			else Pervasives.raise (Already_started fullname)
		with Not_found ->
			let start_time =  Oclock.gettime Oclock.realtime |> toZ in
			map := MapPro.add fullname {e0 with start = Some start_time} !map
	
	let stop : string -> unit
		= fun name ->
		let fullname = get_name name in
		try 
			let e = MapPro.find fullname !map in
			match e.start with
			| None -> Pervasives.raise (Not_started fullname)
			| Some start_time ->
				let time' = Z.sub (Oclock.gettime Oclock.realtime |> toZ) start_time in 
				map := MapPro.add fullname {start = None ; time = Z.add time' e.time} !map
		with Not_found -> Pervasives.raise (Not_started fullname)
end


