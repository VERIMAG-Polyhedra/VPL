all : vpl

coqsrc:
	$(MAKE) -C coq/ DemoExtract.vo

cleancoqsrc:
	$(MAKE) -C coq/ cleanextract
	$(MAKE) -C coq/ DemoExtract.vo

vpl: coqsrc
	cd ocaml; oasis setup; $(MAKE)
#	cd ocaml; oasis setup -setup-update dynamic; $(MAKE)

clean_caml:
	$(MAKE) -C ocaml/ clean
	$(MAKE) -C test/ clean
	rm ocaml/configure ocaml/Makefile ocaml/setup.* ocaml/_tags ocaml/myocamlbuild.ml 

clean_coq:
	$(MAKE) -C coq clean

allclean: clean_caml clean_coq

install: vpl
	$(MAKE) -C ocaml/ install

uninstall: vpl
	$(MAKE) -C ocaml/ uninstall

check:
	$(MAKE) -C test/ check

.PHONY: all coqsrc vpl clean_caml clean_coq allclean install uninstall check cleancoqsrc

