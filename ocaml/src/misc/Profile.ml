module MapPro = Map.Make(struct type t = string let compare = String.compare end)

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
	val start : ?dep:string -> string -> unit
	
	(** Stops the timer for the given function name.
		@raise Not_started if that timer was not launched. *)
	val stop : ?dep:string -> string -> unit
end

module Profile (D : sig val name : string end) = struct
	module D = D
	
	let reset : unit -> unit
		= fun () ->
		map := MapPro.empty
	
	let get_name : ?dep:string -> string -> string
		= fun ?(dep = "") s ->
		if dep = ""
		then String.concat "." [D.name ; s]
		else String.concat "." [D.name ; dep ; s]
		
	let toZ : int64 -> Z.t
		= fun time ->
		Z.of_int64 time
	
	let start : ?dep:string -> string -> unit
		= fun ?(dep = "") name ->
		let fullname = get_name ~dep:dep name in
		try 
			let e = MapPro.find fullname !map in
			if e.start = None
			then let start_time =  Oclock.gettime Oclock.realtime |> toZ in
				map := MapPro.add fullname {e with start = Some start_time} !map
			else Pervasives.raise (Already_started fullname)
		with Not_found ->
			let start_time =  Oclock.gettime Oclock.realtime |> toZ in
			map := MapPro.add fullname {e0 with start = Some start_time} !map
	
	let stop : ?dep:string -> string -> unit
		= fun ?(dep = "") name ->
		let fullname = get_name ~dep:dep name in
		try 
			let e = MapPro.find fullname !map in
			match e.start with
			| None -> Pervasives.raise (Not_started fullname)
			| Some start_time ->
				let time' = Z.sub (Oclock.gettime Oclock.realtime |> toZ) start_time in 
				map := MapPro.add fullname {start = None ; time = Z.add time' e.time} !map
		with Not_found -> Pervasives.raise (Not_started fullname)
		
		
end

module Report = struct
	
	type result = string * Z.t
	type t = result list
	
	let get_results : unit -> t
		= fun () ->
		List.map (fun (name,e) -> (name, e.time))
			(MapPro.bindings !map)
	
	let string_equal : string -> string -> bool
		= fun s1 s2 ->
		String.compare s1 s2 = 0
	
	let is_son_of : string -> string -> bool
		= fun s father ->
		let len_father = String.length father in
		String.length s >= len_father && string_equal father (String.sub s 0 len_father)
	
	let sort : t -> t
		= fun l ->
		let compare (s1,_) (s2,_) = String.compare s1 s2 in
		List.fast_sort compare l
	
	type tree = Node of result * tree list
	
		let time_to_string : Z.t -> string
		= fun t0 ->
		let units = ["ns"; "us"; "ms"; "s" ; "ks" ; "Ms" ; "Bs"] in
		let a = Pervasives.ref 0 in
		let tInt = Pervasives.ref t0 in
		let tDec = Pervasives.ref Z.zero in
		let b = Pervasives.ref true in
		begin
			while !b && !a < List.length units
			do
			  let (tInt', tDec') = Z.div_rem !tInt (Z.of_int 1000) in
			  if Z.equal Z.zero tInt'
			  then b := false
			  else
				 begin
				   tInt := tInt';
				   tDec := tDec';
				   a := !a + 1;
				 end
			done;
			Printf.sprintf
			  "%s.%s%s"
			  (Z.to_string !tInt)
			  (Z.div !tDec (Z.of_int 10) |> Z.to_string)
			  (List.nth units !a);
		end
	
	let result_to_string : result -> string
		= fun (name,time) ->
		Printf.sprintf "%s -> %s" name (time_to_string time)
		
	let rec tree_to_string : tree -> string
		= function
		| Node (res, t) -> Printf.sprintf "%s\n%s"
			(result_to_string res)
			(String.concat "\n" (List.map tree_to_string t) |> Misc.add_tab 1 ) 
	
	let rec add : result -> tree -> tree * bool
		= fun (s, time) -> function
		| Node ((root,time_root), trees) as tree -> 
			if is_son_of s root
			then 
				let (trees', b) = List.fold_left
					(fun (trees,b) tree ->
						if b then (tree :: trees, b)
						else 
							let (t',b') = add (s, time) tree in
							(t' :: trees, b || b'))
					([], false) trees 
				in
				if b
				then (Node ((root,time_root), trees'), true)
				else 
					let tree_son = Node ((s, time),[]) in 
					(Node ((root,time_root), tree_son :: trees), true)
			else (tree, false)
	
	let init_tree : unit -> tree
		= fun () ->
		Node (("", Z.zero), [])
		
	let build : t -> tree
		= fun l ->
		List.fold_left
			(fun tree e ->
				let (tree',b) = add e tree in
				if b then tree'
				else Pervasives.failwith "build")
			(init_tree()) l
			
end

let l = [
	"b", Z.zero;
	"a.b", Z.zero;
	"a.d", Z.zero;
	"a.b.c", Z.zero;
	"a", Z.zero;
];;

Report.sort l
	|> Report.build 
	|> Report.tree_to_string
	|> print_endline
	;;


