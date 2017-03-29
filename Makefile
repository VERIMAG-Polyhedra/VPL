all : vpl

coqsrc:
	$(MAKE) -C coq/ DemoExtract.vo

cleancoqsrc:
	$(MAKE) -C coq/ cleanextract
	$(MAKE) -C coq/ DemoExtract.vo

vpl: coqsrc
	cd ocaml; $(MAKE)

clean_caml:
	$(MAKE) -C ocaml/ clean
	$(MAKE) -C test/ clean
	rm ocaml/setup.data setup.log ocaml/_tags ocaml/myocamlbuild.ml 

clean_coq:
	$(MAKE) -C coq clean

to_opam:
	cd ocaml
	oasis2opam --local

allclean: clean_caml clean_coq

install: vpl
	$(MAKE) -C ocaml/ install

uninstall: vpl
	$(MAKE) -C ocaml/ uninstall

check:
	$(MAKE) -C test/ check

.PHONY: all coqsrc vpl clean_caml clean_coq allclean install uninstall check cleancoqsrc

