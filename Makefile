# this configuration file is generated from configure.make
include Makefile.config

SRC = -I src -I src/datatypes -I src/core -I src/lin -I src/misc -I src/plp \
	-I src/lin/handelman -I src/lin/handelman/oracle -I src/lin/intervalization -I src/smt -I src/lin/poly \
	-I src/interface $(COQSRC)

TEST = -I test -I test/datatypes -I test/lin -I test/plp -I test/core -I test/lin/handelman -I test/lin/handelman/oracle
INCLUDES = $(SRC) $(TEST)

OCAMLDOC_FLAGS = -hide-warnings
OCB_FLAGS = -use-ocamlfind $(INCLUDES)

clean:
	$(OCB) -clean
	rm -f Makefile.config 
	rm -f META
	rm -f src/core/MinLP.ml

allclean: coqclean clean

check:
	$(OCB) All_t.native
	./_build/test/All_t.native

vpl: vpl.cmo 
	$(OCB) vpl.cma
	$(OCB) vpl.cmxa
	$(OCB) vpl.cmxs

# check that packages can be found
sanity:
	ocamlfind query zarith 
	ocamlfind query unix

#slave:
#	$(OCB) PLPSlave.native

doc:
	$(OCB) -docflag $(OCAMLDOC_FLAGS) vpl.docdir/index.html

java : 
	ocamljava $(OCB) -o vpl.jar Pol.ml
	
configure: Makefile.config

# proxy rule for rebuilding configuration files directly from the main Makefile
Makefile.config:
	$(MAKE) -f configure.make all

TOINSTALL := _build/vpl.cmo _build/vpl.cmi _build/vpl.cma _build/vpl.cmxa _build/vpl.a _build/vpl.cmxs _build/vpl.cmx _build/vpl.mlpack

install:
	ocamlfind install vpl META $(TOINSTALL)

uninstall:
	ocamlfind remove vpl

.PHONY: all clean vpl cvpl sanity check doc coqsrc demo_vplcoq_debug demo_vplcoq coqclean allclean install uninstall configure check_glpk save_coq_extraction
