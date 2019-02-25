module AbstractDomainMakerQ = TrackedDomain.MakeAbstractDomain(Scalar.Rat)
module AbstractDomainMakerZ = TrackedDomain.MakeAbstractDomain(Scalar.Int)

module InterfaceQ = WrapperTraductors.Interface(Scalar.Rat)
module InterfaceZ = WrapperTraductors.Interface(Scalar.Int)

module UnitQ = struct
    module Coeff = Scalar.Rat
    module HighLevel = NCDomain.NCVPL_Unit.Q
    module AD = AbstractDomainMakerQ.Make(HighLevel)
    include TrackedDomain.Make(AD)
end

module UnitZ = struct
    module Coeff = Scalar.Int
    module HighLevel = NCDomain.NCVPL_Unit.Z
    module AD = AbstractDomainMakerZ.Make(HighLevel)
    include TrackedDomain.Make(AD)
end

module CstrQ = struct
    module Coeff = Scalar.Rat
    module HighLevel = NCDomain.NCVPL_Cstr.Q
    module AD = AbstractDomainMakerQ.Make(HighLevel)
    include TrackedDomain.Make(AD)
end

module CstrZ = struct
    module Coeff = Scalar.Int
    module HighLevel = NCDomain.NCVPL_Cstr.Z
    module AD = AbstractDomainMakerZ.Make(HighLevel)
    include TrackedDomain.Make(AD)
end

module CoqQ = struct
    module Coeff = Scalar.Rat
    module AD = AbstractDomainMakerQ.Make(CDomain.PedraQWrapper)
    include TrackedDomain.Make(AD)
end

module CoqZ = struct
    module Coeff = Scalar.Int
    module AD = AbstractDomainMakerZ.Make(CDomain.PedraZWrapper)
    include TrackedDomain.Make(AD)
end
