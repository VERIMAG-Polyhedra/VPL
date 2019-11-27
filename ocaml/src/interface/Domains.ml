(**
Available polyhedral domains.
The user interface functor must be provided with one of them.
There are three levels of certification: {ul
    {- {b No certification}: no certificate is produced}
    {- {b OCaml certification}: Each operator of the domain produces a certificate, ie a witness of its computation that can be checked.
    }
    {- {b Coq certification}: In addition to guarantees offered by OCaml certification, certificates are here extracted from Coq proven types.
    }
}
*)

(**/**)
module AbstractDomainMakerQ = TrackedDomain.MakeAbstractDomain(Scalar.Rat)
module AbstractDomainMakerZ = TrackedDomain.MakeAbstractDomain(Scalar.Int)

module InterfaceQ = WrapperTraductors.Interface(Scalar.Rat)
module InterfaceZ = WrapperTraductors.Interface(Scalar.Int)
(**/**)

(** Uncertified domain on Q. *)
module UnitQ = struct
    (**/**)
    module Coeff = Scalar.Rat
    module HighLevel = NCDomain.NCVPL_Unit.Q
    module AD = AbstractDomainMakerQ.Make(HighLevel)
    include TrackedDomain.Make(AD)
    (**/**)
end

(** Uncertified domain on Z. *)
module UnitZ = struct
    (**/**)
    module Coeff = Scalar.Int
    module HighLevel = NCDomain.NCVPL_Unit.Z
    module AD = AbstractDomainMakerZ.Make(HighLevel)
    include TrackedDomain.Make(AD)
    (**/**)
end

(** OCaml certified domain on Q. *)
module CstrQ = struct
    (**/**)
    module Coeff = Scalar.Rat
    module HighLevel = NCDomain.NCVPL_Cstr.Q
    module AD = AbstractDomainMakerQ.Make(HighLevel)
    include TrackedDomain.Make(AD)
    (**/**)
end

(** OCaml certified domain on Z. *)
module CstrZ = struct
    (**/**)
    module Coeff = Scalar.Int
    module HighLevel = NCDomain.NCVPL_Cstr.Z
    module AD = AbstractDomainMakerZ.Make(HighLevel)
    include TrackedDomain.Make(AD)
    (**/**)
end

(** Coq certified domain on Q. *)
module CoqQ = struct
    (**/**)
    module Coeff = Scalar.Rat
    module AD = AbstractDomainMakerQ.Make(CDomain.PedraQWrapper)
    include TrackedDomain.Make(AD)
    (**/**)
end

(** Coq certified domain on Z. *)
module CoqZ = struct
    (**/**)
    module Coeff = Scalar.Int
    module AD = AbstractDomainMakerZ.Make(CDomain.PedraZWrapper)
    include TrackedDomain.Make(AD)
    (**/**)
end
