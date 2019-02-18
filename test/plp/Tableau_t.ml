open Vpl
open Tableau

let equal tab1 tab2 =
    nRows tab1 = nRows tab2 &&
    nCols tab1 = nCols tab2 && (
        let r = ref true in
        for i = 0 to (nRows tab1) - 1 do
            for j = 0 to (nCols tab1) - 1 do
                if get i j tab1 <> get i j tab2
                then r := false
            done;
        done;
        !r
    )

let mk : (int*int) array array -> t
    = fun a ->
    Array.map (Array.map (fun (num,den) -> Scalar.Rat.mk den num)) a

let pivot_ts : Test.t
    =  fun () ->
    let chk (nm, row, col, inM, em) st =
        let am = copy inM in
        pivot row col am;
        Test.equals nm (fun _ -> "unimplemented") equal em am st
        in
    let tcs = [
        "one_coeff", 0, 0,
        mk [|
            [|-2,1|];
        |],
        mk [|
            [|1,1|];
        |]
    ;
        "one_row", 0, 0,
        mk [|
            [|2,1; 1,1|];
        |],
        mk [|
            [|1,1; 1,2|];
        |]
    ;
        "one_col", 1, 0,
        mk [|
            [|-3,2|];
            [|2,1|];
            [|0,1|];
            [|3,4|];
        |],
        mk [|
            [|0,1|];
            [|1,1|];
            [|0,1|];
            [|0,1|];
        |]
    ;
        "regular", 0, 0,
        mk [|
            [|2,1; 1,1|];
            [|1,1; 0,1|]
        |],
        mk [|
            [|1,1; 1,2|];
            [|0,1; -1,2|]
        |]
    ] in
    Test.suite "pivot" (List.map chk tcs)

let add_col_ts : Test.t
    = fun () ->
    let chk (nm, inM, em) st =
        let init_f i = Scalar.Rat.of_int i in
        let am = addCol init_f inM in
        Test.equals nm (fun _ -> "unimplemented") equal em am st
    in
    let tcs = [
        "one_coeff",
        mk [|
            [|-2,1|];
        |],
        mk [|
            [|0,1; -2,1|];
        |]
        ;
        "one_row",
        mk [|
            [|2,1; 1,1|];
        |],
        mk [|
            [|2,1; 0,1; 1,1|];
        |]
        ;
        "one_col",
        mk [|
            [|-3,2|];
            [|2,1|];
            [|0,1|];
            [|3,4|];
        |],
        mk [|
            [|0,1; -3,2|];
            [|1,1; 2,1|];
            [|2,1; 0,1|];
            [|3,1; 3,4|];
        |]
        ;
        "regular",
        mk [|
            [|2,1; 1,1|];
            [|1,1; 0,1|]
        |],
        mk [|
            [|2,1; 0,1; 1,1|];
            [|1,1; 1,1; 0,1|]
        |]
    ] in
    Test.suite "addCol" (List.map chk tcs)

let rem_col_ts : Test.t
    = fun () ->
    let chk (nm, i_col, inM, em) st =
        let am = remCol i_col inM in
        Test.equals nm (fun _ -> "unimplemented") equal em am st
    in
    let tcs = [
        "one_coeff", 0,
        mk [|
            [|1,1;|];
        |],
        mk [|
            [||];
        |]
        ;
        "one_row", 1,
        mk [|
            [|2,1; 0,1; 1,1|];
        |],
        mk [|
            [|2,1; 1,1|];
        |]
        ;
        "one_col", 0,
        mk [|
            [|-3,2|];
            [|2,1|];
            [|0,1|];
            [|3,4|];
        |],
        mk [|
            [||];
            [||];
            [||];
            [||];
        |]
        ;
        "regular", 1,
        mk [|
            [|2,1; 1,1|];
            [|1,1; 0,1|]
        |],
        mk [|
            [|2,1;|];
            [|1,1;|]
        |]
    ] in
    Test.suite "remCol" (List.map chk tcs)

let ts : Test.t
  =  fun () ->
  Test.suite "Tableau" [
    pivot_ts();
    add_col_ts();
    rem_col_ts();
	]
