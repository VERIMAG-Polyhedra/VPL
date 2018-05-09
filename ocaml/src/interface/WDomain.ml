open NCDomain


module P = struct
	include MakePolyhedronDomain (Factory.Farkas)

	(** Careful : addNLM is UNcertified. *)
	let addNLM : t -> CP.t list -> t
		= fun p cps ->
		failwith "unimplemented"
end

module I = NCInterface.Lift (P)
module I_Q = I.QInterface
module QHighLevelDomain = CW.MakeHighLevel (I_Q)

(* Interface du HighlevelDomain dans interface/CWrappers *)
