open Vpl
open IOtypes

module MakeTests(T : Type) = struct

    open T

    module P = D.Poly
    module M = D.Poly.Monomial
    module MB = D.Poly.MonomialBasis

    let x = Var.fromInt 1
    let y = Var.fromInt 2
    let z = Var.fromInt 3

    let coeff : int -> D.N.t
        = fun i ->
        P.Coeff.of_int i
        |> P.coeff_to_n

    let coeff_opt : int option -> D.N.t option
        = function
        | Some i -> Some (coeff i)
        | None -> None

    let env_list = [
        x, Some (-3), Some 4;
        y, Some 1, Some 1;
        z, None, Some (-1)
    ]

    let env var =
        try
            let (_,low,up) = List.find (fun (v, low, up) ->
                Var.equal v var
            ) env_list in
            D.NoneItv.mk (coeff_opt low) (coeff_opt up)
        with Not_found -> D.NoneItv.mk None None


    let ts : Test.t
        = fun () ->
        Test.suite "Itv" []

end

let ts : Test.t
    = fun () ->
    List.map (fun m ->
        let module M = (val m : Type) in
        let module M2 = MakeTests(M) in
        M2.ts()
    ) [ (module DZ);
        (module DQ);
    ]
    |>
    Test.suite "Intervalization"
