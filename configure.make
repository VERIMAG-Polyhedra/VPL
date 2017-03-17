SHELL := /bin/bash
COQ_EXPECTED_VERSION := "8.6"

include $(shell ocamlc -where)/Makefile.config

all: check_glpk build_meta Makefile.config

check_glpk:
	@{ \
	echo "Looking for Glpk"; \
	if test -z "$$GLPK_PATH"; then GLPK_PATH=$$(ocamlfind query glpk); fi; \
	if [ -n "$$GLPK_PATH" ]; then \
		echo "Glpk found"; \
		cp ./src/core/MinLP.ok_glpk.ml ./src/core/MinLP.ml; \
	else \
		echo "Glpk not found"; \
		cp ./src/core/MinLP.ko_glpk.ml ./src/core/MinLP.ml; \
	fi \
	} 

build_meta:
	@{ \
	echo "Looking for Glpk"; \
	if test -z "$$GLPK_PATH"; then GLPK_PATH=$$(ocamlfind query glpk); fi; \
	if [ -n "$$GLPK_PATH" ]; then \
		echo "Glpk found"; \
		(echo "name=\"vpl\""; \
		echo "description=\"verified polyhedra library\""; \
		echo "version=\"2.0\""; \
		echo "requires=\"unix,zarith,glpk\""; \
		echo "archive(byte)=\"vpl.cma\""; \
		echo "archive(native)=\"vpl.cmxa\"") > META; \
	else \
		echo "Glpk not found"; \
		(echo "name=\"vpl\""; \
		echo "description=\"verified polyhedra library\""; \
		echo "version=\"2.0\""; \
		echo "requires=\"unix,zarith\""; \
		echo "archive(byte)=\"vpl.cma\""; \
		echo "archive(native)=\"vpl.cmxa\"") > META; \
	fi \
	}

Makefile.config:
	@(if [ "$(shell coqc -v | head -1 | sed -e 's/.*version \([0-9].[0-9]\).*/\1/g' 2> /dev/null)" == "${COQ_EXPECTED_VERSION}" ] ; then \
		echo "# This file was generated from configure.make for coqc-${COQ_EXPECTED_VERSION}"; \
		echo ; \
		echo "COQSRC = -I src/coq_ml -I src/coq_already_extracted"; \
		echo ;\
		if test -z "$$GLPK_PATH"; then GLPK_PATH=$$(ocamlfind query glpk); fi; \
		if [ -n "$$GLPK_PATH" ]; then \
			echo "OCB = ocamlbuild -package glpk \$$(OCB_FLAGS)"; \
		else \
			echo "OCB = ocamlbuild \$$(OCB_FLAGS)"; \
		fi; \
		echo ;\
		echo "all : vpl check"; \
		echo ;\
		echo "vpl.cmo: coqsrc save_coq_extraction sanity"; \
		echo "	\$$(OCB) vpl.cmo"; \
		echo ;\
		echo "# The 5 following targets are Coq compilation targets !"; \
		echo "coqclean:"; \
		echo "	\$$(MAKE) -C src/coq clean"; \
		echo ;\
		echo "coqsrc: sanity"; \
		echo "	\$$(MAKE) -C src/coq DemoExtractTests.vo"; \
		echo ; \
		echo "demo_vplcoq_debug: coqsrc"; \
		echo "	\$$(OCB) -cflag -g -lflag -g demo_vplcoq.byte"; \
		echo "	OCAMLRUNPARAM=b ./demo_vplcoq.byte"; \
		echo ; \
		echo "save_coq_extraction: coqsrc"; \
		echo "	rsync -av -del ./src/coq/extracted/ ./src/coq_already_extracted"; \
		echo ; \
		echo "demo_vplcoq: save_coq_extraction"; \
		echo "	\$$(OCB) demo_vplcoq.native"; \
		echo "	./demo_vplcoq.native"; \
	else \
		echo "# This file was generated from configure.make"; \
		echo "# WARNING coqc-${COQ_EXPECTED_VERSION} not found!"; \
		echo ; \
		echo "COQSRC = -I src/coq_ml -I src/coq_already_extracted"; \
		echo ;\
		if test -z "$$GLPK_PATH"; then GLPK_PATH=$$(ocamlfind query glpk); fi; \
		if [ -n "$$GLPK_PATH" ]; then \
			echo "OCB = ocamlbuild -package glpk \$$(OCB_FLAGS)"; \
		else \
			echo "OCB = ocamlbuild \$$(OCB_FLAGS)"; \
		fi; \
		echo ;\
		echo "all : vpl check"; \
		echo ;\
		echo "vpl.cmo: sanity"; \
		echo "	\$$(OCB) vpl.cmo"; \
	fi; \
	) > $@
