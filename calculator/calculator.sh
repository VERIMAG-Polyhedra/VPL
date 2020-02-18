#!/bin/bash

OCAMLFIND=ocamlfind
VPLPATH=$($OCAMLFIND query vpl-core)

ocaml -I $VPLPATH $1
