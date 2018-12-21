module type Type = sig
	module Vec : Vector.Type

	type typT = Param | Var | Slack

	module Usr : sig
		type t = typT * Var.t

		val equal : t -> t -> bool
	end

	type t

	val empty : t

	(** [equal m m'] returns true if [m] and [m'] are syntactically equal. *)
	val equal : t -> t -> bool

	(** [alloc t x m] allocates [x] in the next available slot. *)
	val alloc : typT -> Var.t -> t -> t

	(** [allocAt t x i m] states that [x] is to be found at column
	[i] in [m].  If [x] is already allocated or if there is already
	something at this column, then [Invalid_argument] is raised.
	If there is a gap between the last allocated index and [i],
	[Invalid_argument] is raised as well. *)
	val allocAt : typT -> Var.t -> int -> t -> t

	(** [mkVar l m] treats all the elements of [l] as variables.
	They are allocated in [m] consecutively and starting from 0.
	If variables have already been allocated in [m], the function
	raises an [Invalid_argument] exception. *)
	val mkVar : Var.t list -> t -> t

	(** [mkParam l m] treats all the elements of [l] as parameters.
	They are allocated in [m] consecutively and starting from 0.
	If parameters have already been allocated in [m], the function
	raises an [Invalid_argument] exception.
	The allocation of VPL variables coincidentally happens to match
	that performed by [Context]. *)
	val mkParam : Var.t list -> t -> t

	(** [to_index m t x] returns the column which has been assigned
	to variable or parameter (depending on [t]) [x].  If [x] is
	not assigned to anything, then [Invalid_argument] is raised. *)
	val to_index : t -> typT -> Var.t -> int

	(** [to_user m t i] returns the user-provided variable, slack or
	parameter (depending on [t]) of the given index. *)
	val to_user : t -> typT -> int -> Var.t * typT

	(** [to_vpl m i] returns the VPL representation of the
	parameter at index [i]. *)
	val to_vpl : t -> int -> Var.t

	(** [vpl_max m] returns a [Var.t] such that all parameters
	are assigned strictly smaller [Var.t]'s. *)
	val vpl_max : t -> Var.t

  	val slack_max : t -> Var.t

	(** [allocSlackShift x m] allocates [x] at index [0], shifting
	all the variables and slack accordingly.  If [x] is already allocated,
	[Invalid_argument] is raised. *)
	val allocSlackShift : Var.t -> t -> t

	(** [freeSlackShift x m] removes slack [x] from [m] and shifts
	variables as needed.  If [x] isn't already allocated, then
	[Invalid_argument] is raised. *)
	val freeSlackShift : Var.t -> t -> t

    val to_string: t -> typT -> int -> string
end

module Naming (Vec : Vector.Type) = struct
	module Vec = Vec

	type typT = Param | Var | Slack

	module Usr = struct
		type t = typT * Var.t

		let compareTyp : typT -> typT -> int
			= fun t1 t2 ->
			match (t1,t2) with
			| Var, Var -> 0
			| Param, Param -> 0
			| Slack, Slack -> 0
			| Var, _ -> 1
			| Param, Var -> -1
			| Param, Slack -> 1
			| Slack, Var -> -1
			| Slack, Param -> -1

		let compare : t -> t -> int
			= fun (t1,v1) (t2,v2) ->
			let r = Var.cmp v1 v2 in
			if r = 0
			then compareTyp t1 t2
			else r

		let equal : t -> t -> bool
			= fun (t1,u1) (t2,u2) -> u1 = u2 && t1 = t2

	end

	module UserMap = Map.Make (Usr)

	module PrivMap = Map.Make (struct type t = int let compare = Pervasives.compare end)

	type paramT = Var.t
	type varT = Var.t
	type slackT = Var.t

	type t = {
		nxtIdVar : int;
		nxtIdParam : int;
		nxtParam : Var.t;
	  	nxtVar : Var.t;
	  	nxtSlack : Var.t;
	  	usrMap : int UserMap.t;
	  	varMap : Usr.t PrivMap.t;
	  	paramMap : Usr.t PrivMap.t;
	}

	let empty : t
	  = {
	  nxtIdVar = 0;
	  nxtIdParam = 0;
	  nxtParam = Var.u;
	  nxtVar = Var.u;
	  nxtSlack = Var.u;
	  usrMap = UserMap.empty;
	  varMap = PrivMap.empty;
	  paramMap = PrivMap.empty;
	}

	let equal : t -> t -> bool
		= fun m m' ->
	  	UserMap.equal (=) m.usrMap m'.usrMap
	  	&& PrivMap.equal Usr.equal m.varMap m'.varMap
	  	&& PrivMap.equal Usr.equal m.paramMap m'.paramMap
	  	&& m.nxtIdVar = m'.nxtIdVar
	 	&& m.nxtIdParam = m'.nxtIdParam
	  	&& Var.equal m.nxtParam m'.nxtParam
	  	&& Var.equal m.nxtVar m'.nxtVar
		&& Var.equal m.nxtSlack m'.nxtSlack

	let new_max : Var.t -> Var.t -> Var.t
		= fun v1 v2 ->
		if Var.cmp v1 v2 > 0
		then Var.next v1
		else Var.next v2

	let alloc : typT -> Var.t -> t -> t
		= let allocParam : Var.t -> t -> t
			= fun x m ->
		   let id = m.nxtIdParam in
		   {m with
			usrMap = UserMap.add (Param,x) id m.usrMap;
			paramMap = PrivMap.add id (Param,x) m.paramMap;
			nxtIdParam = id+1;
			nxtParam = new_max m.nxtParam x
		   }
		in
		let allocV : Var.t -> t -> t
		   = fun x m ->
		   let id = m.nxtIdVar in
		   {m with
			usrMap = UserMap.add (Var,x) id m.usrMap;
			varMap = PrivMap.add id (Var,x) m.varMap;
			nxtIdVar = id+1;
			nxtVar = new_max m.nxtVar x
		   }
		in
		let allocSlack : Var.t -> t -> t
		   = fun x m ->
		   let id = m.nxtIdVar in
		   {m with
			usrMap = UserMap.add (Slack,x) id m.usrMap;
			varMap = PrivMap.add id (Slack,x) m.varMap;
			nxtIdVar = id+1;
			nxtSlack = new_max m.nxtSlack x
		   }
		in
		fun t x m ->
		if UserMap.mem (t,x) m.usrMap
		then Pervasives.invalid_arg "Naming.alloc"
		else
		   match t with
		   | Param -> allocParam x m
		   | Var -> allocV x m
			| Slack -> allocSlack x m

	let allocAt : typT -> Var.t -> int -> t -> t
		= fun t x i m ->
	  	if UserMap.mem (t,x) m.usrMap
	  	 ||
			(t = Var && (PrivMap.mem i m.varMap) )
		 ||
			(t = Param && (PrivMap.mem i m.paramMap))
		 ||
			(t = Slack && (PrivMap.mem i m.varMap))
	  then Pervasives.invalid_arg "Naming.allocAt"
	  else alloc t x m

	(* il s'agit de la variable d'ajout, qui fait parti des slacks*)
	let allocSlackShift : Var.t -> t -> t
		= fun x m ->
		if UserMap.mem (Slack,x) m.usrMap
		then Pervasives.invalid_arg "Naming.allocSlackShift"
		else
			{m with
		   usrMap = UserMap.mapi
		   	(fun (t,_) i -> if t = Var || t = Slack
		   		then i+1
		   		else i)
		   	m.usrMap
				|> UserMap.add (Slack,x) 0
			;
			varMap = PrivMap.fold
				(fun i x' -> PrivMap.add (i + 1) x')
				m.varMap
				(PrivMap.singleton 0 (Slack,x))
			;
			nxtIdVar = m.nxtIdVar + 1
		}

	let freeSlackShift : Var.t -> t -> t
		= fun x m ->
		if not (UserMap.mem (Slack,x) m.usrMap)
		then Pervasives.invalid_arg "Naming.free: does not exist"
	  	else
			let id = UserMap.find (Slack,x) m.usrMap in
			{m with
			usrMap = UserMap.mapi
				(fun (t,_) id' -> if t = Var || t = Slack && id' > id
					then id' - 1
					else id')
				m.usrMap
	  			|> UserMap.remove (Slack,x)
	  		;
			varMap = PrivMap.fold
		 		(fun i x m' ->
		 		if i = id then m'
		  		else
				if i > id then PrivMap.add (i - 1) x m'
				else PrivMap.add i x m')
		 		m.varMap PrivMap.empty
		 	;
			nxtIdVar = m.nxtIdVar - 1;
	   }

	let mkVar : Var.t list -> t -> t
  		= fun l m ->
  		if Var.cmp m.nxtVar Var.u <> 0
  		then Pervasives.invalid_arg "Naming.mkVar"
  		else List.fold_left (fun m x -> alloc Var x m) m l

	let mkParam : Var.t list -> t -> t
  		= fun l m ->
  		if Var.cmp m.nxtParam Var.u <> 0
  		then Pervasives.invalid_arg "Naming.mkParam"
  		else List.fold_left (fun m x -> alloc Param x m) m l

	let to_user : t -> typT -> int -> Var.t * typT
  		= fun m t i ->(*
  		Printf.sprintf "to_user " ^ (match t with |Param -> "Param " |Var -> "Var ") ^ (string_of_int i)
  			|> print_endline;*)
  		match t with
  		| Var | Slack ->
     		if PrivMap.mem i m.varMap
     		then let (t,v) = PrivMap.find i m.varMap in
     			if t = Var || t = Slack then (v,t)
     			else Pervasives.failwith "Naming.to_user:var"
     		else Pervasives.invalid_arg "Naming.to_user"
  		| Param ->
     		if PrivMap.mem i m.paramMap
     		then let (t,v) = PrivMap.find i m.paramMap in
     			if t = Param then (v,t)
     			else Pervasives.failwith "Naming.to_user:param"
     		else Pervasives.invalid_arg "Naming.to_user"

	let user_to_string: typT -> Var.t -> string
  		= fun t u ->
  		(match t with Param -> "p" | Var -> "d" | Slack -> "s") ^ Var.to_string u

	let to_string: t -> typT -> int -> string
  		= fun m t i ->
  		try
  			let (v,t) = to_user m t i
  			in user_to_string t v
  		with Invalid_argument _ -> "??"

	let to_index: t -> typT -> Var.t -> int
  		= fun m t x ->
  		try
  			UserMap.find (t,x) m.usrMap
  		with Not_found ->
  			Pervasives.failwith "Naming.to_index"

(*
	let vpl_to_index : t -> Var.t -> int
  		= fun m x ->
  		PrivMap.fold
    		(fun i p r ->
     		match r with
     		| Some _ as r' -> r'
     		| None -> if Var.cmp x p = 0 then Some i else None)
    		m.paramMap None
  		|> function
    		| None -> Pervasives.invalid_arg "Naming.vpl_to_index"
    		| Some i -> i

	let vpl_to_user : t -> Var.t -> Var.t
  		= fun m x ->  PrivMap.find (vpl_to_index m x) m.paramMap

	let vpl_to_string : t -> Var.t -> string
  		= fun m x -> vpl_to_user m x |> user_to_string Param
*)
	let to_vpl : t -> int -> Var.t
  		= fun m i ->
  		try
  			PrivMap.find i m.paramMap
  			|> Pervasives.snd
  		with Not_found ->
  			Pervasives.failwith "Naming.to_index"

	let vpl_max : t -> Var.t
  		= fun m -> m.nxtParam

  	let slack_max : t -> Var.t
  		= fun m -> m.nxtSlack

end
