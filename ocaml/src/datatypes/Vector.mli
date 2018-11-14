open Vector_type

module type Type = Type

module Rat : sig
    module Positive : Type with module Coeff = Scalar.Rat and module M = Rtree and module V = Var.Positive

    module Int : Type with module Coeff = Scalar.Rat and module M = VarMap.VarMap(Var.Int) and module V = Var.Int
end

module Float : sig
    module Positive : Type with module Coeff = Scalar.Float and module M = Rtree and module V = Var.Positive

    module Int : Type with module Coeff = Scalar.Float and module M = VarMap.VarMap(Var.Int) and module V = Var.Int
end

module Symbolic : sig
    module Positive : Type with module Coeff = Scalar.Symbolic and module M = Rtree and module V = Var.Positive

    module Int : Type with module Coeff = Scalar.Symbolic and module M = VarMap.VarMap(Var.Int) and module V = Var.Int
end

module Int : sig
    module Positive : Type with module Coeff = Scalar.Int and module M = Rtree and module V = Var.Positive

    module Int : Type with module Coeff = Scalar.Int and module M = VarMap.VarMap(Var.Int) and module V = Var.Int
end

module MachineInt : sig
    module Positive : Type with module Coeff = Scalar.MachineInt and module M = Rtree and module V = Var.Positive

    module Int : Type with module Coeff = Scalar.MachineInt and module M = VarMap.VarMap(Var.Int) and module V = Var.Int
end
