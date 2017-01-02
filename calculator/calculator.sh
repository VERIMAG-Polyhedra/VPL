#!/bin/bash
if [ ! -d temp ];
then
	mkdir temp
fi

OCAMLFIND=ocamlfind
VPLPATH=$($OCAMLFIND query vpl)

ocaml -I $VPLPATH

files=(temp/*)
if [ ${#files[@]} -gt 0 ] #if directory contains file(s)
then
	for file in $files
	do
		rm $file
	done
fi

rmdir temp
