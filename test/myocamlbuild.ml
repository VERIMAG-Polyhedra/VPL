open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | After_rules ->
       ocaml_lib ~extern:true ~dir:"../../ocaml/_build/src/" "vpl"
  | _ -> ()
end;;
