open ASTerm
open ImpureConfig
open Map_poly
open NumC
open PedraQBackend

(** val oracle : linearizeContext -> ZTerm.t Core.Base.imp **)

let oracle = IOracle.oracle

(** val handelman_oracle :
    t -> cmpG -> QTerm.t -> Handelman_compute.certif list **)

let handelman_oracle = HOracle_backend.oracle
