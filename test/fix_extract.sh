#!/bin/bash
# insert "open Vpl" at the head of extracted files 
#

HEAD="open Vpl"

function myfix() {
    if ! [ -f $1 ]; then
        echo "ERROR: $1 does not exist"
        exit 1
    fi
    if !(head -1 $1 | grep -q "${HEAD}"); then
        echo "fix $1"
        mv $1 tmp.$$
        echo "${HEAD}" > $1
        cat tmp.$$ >> $1
        rm -f tmp.$$
    fi
}

cd coq_extracted || exit 1
myfix "DemoPLTests.ml" || exit 1
# myfix "DemoPLVerifier.ml" || exit 1
