(** Interface of linearization Oracles. *)

module Debug = DebugTypes.Debug(struct let name = "Oracle" end)

(** Interface of linearization Oracles.
    An Oracle has a modular design: strategies are encoded within Prayers, that are ordered.
    Starting from a polynomial to treat, each prayer is tried one after the other.
    If it can be applied, the prayer updates the polynomial until it begins 0.*)
module type OracleType = sig

    (** Module of polynomials *)
    module P : Poly.Type

    (** Prophecy made by the Oracle. *)
    type prophecy

    (** Prayer interface. *)
    module type Prayer = sig

        (** Prayer Name. *)
        val name : string

        (** Type of elements needed by the prayer. *)
        type pneuma

        (** If set to true, the prayer won't be tried again if it fails once. *)
        val kill_when_fail : bool

        (** Praying for a pneuma to come.
            This is the parsing step of the strategy.
            The prayer determines if it can apply on the polynomial.
            @param p the polynomial to handle
            @param pr the current prophecy
            @return None if the prayer can not be applied
            @return Some [pneuma] if the prayer can be applied *)
        val pray : P.t -> prophecy -> pneuma option

        (** Inhale the pneuma to generate a prophecy.
            If the prayer returned a pneuma, it is provided to this function that executes the strategy.
            @param p the polynomial to handle
            @param the current prophecy
            @return p' the new polynomial to handle
            @return pr' the new prophecy *)
        val inhale : P.t -> prophecy -> pneuma -> P.t * prophecy
    end

    (** List of prayers.
        There are applied in this order. *)
    val prayers : (module Prayer) list

end

(** Builds a runnable oracle from an {!modtype:OracleType}. *)
module Make(Oracle : OracleType) = struct
    include Oracle

    (** List of prayers used in the oracle.
        It is initialized with {!val:prayers}. *)
    let my_prayers : ((module Prayer) list) ref
        = ref []

    let remove_prayer : (module Prayer) -> unit
        = fun m ->
        my_prayers := Misc.pop (fun m m' ->
            let module M = (val m : Prayer) in
            let module M' = (val m' : Prayer) in
            String.equal M'.name M.name
        ) !my_prayers m

    (** Runs the oracle until the polynomial is 0.
        Prayers are tried in the order given by {!val:prayers}.
        If a prayer is applicable, the oracle starts back with the initial list of prayers.
        @param p the polynomial to handle
        @param pr the current prophecy
        @param prayers the current prayers list
        @return the new prophecy *)
    let rec make_prophecy : P.t -> prophecy -> (module Prayer) list
    -> prophecy
        = fun p pr -> function
        | [] -> make_prophecy p pr !my_prayers
        | prayer :: l ->
            let module M = (val prayer : Prayer) in
            match M.pray p pr with
            | None -> begin
                if M.kill_when_fail
                then remove_prayer prayer;
                make_prophecy p pr l
                end
            | Some pneuma -> begin
                Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Praying for prophecy %s on polynomial %s.
                And God said it was good."
                    M.name
                    (P.to_string p)));
                let (p', pr') = M.inhale p pr pneuma in
                if P.isZ p'
                then pr'
                else begin
                    Debug.log DebugTypes.Normal (lazy (Printf.sprintf "Continuing on %s" (P.to_string p')));
                    make_prophecy p' pr' !my_prayers
                    end
                end

    (** Runs the oracle.
        @param init_prophecy the initial prophecy
        @param p the polynomial to handle
        @return the prophecy *)
    let run : prophecy -> P.t -> prophecy
        = fun init_prophecy p ->
        my_prayers := prayers;
        make_prophecy p init_prophecy prayers
end
