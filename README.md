# VPL (Verified Polyhedra Library)

## GENERAL INFORMATIONS

The VPL is an Ocaml library allowing to compute with convex polyhedra.
It provides standard operators -- certified in Coq -- to use this library as an abstract domain of polyhedra.

Contributors: Alexis Fouilhé, Alexandre Maréchal, Sylvain Boulmé, Hang Yu, Michaël Périn, David Monniaux.
Developed at Verimag and supported by ANR Verasco and ERC Stator.

If you find a bug or have any comment, feel free to contact us at verimag-polyhedra-developers@univ-grenoble-alpes.fr

## INSTALLATION

1. __From [opam](https://opam.ocaml.org/)__

    First, add the following repository in your opam system:

        opam repo add vpl https://raw.githubusercontent.com/VERIMAG-Polyhedra/opam-vpl/master

    Then, install the following packages (depending on your needs):

    * `vpl-core`: the ocaml library

      ```
      opam install vpl-core
      ```

    * `coq-vpl`: the Coq library (only needed to get Coq proofs about VPL operators)

      ```
      opam install coq-vpl
      ```

    * `coq-vpltactic`: the [VplTactic](https://github.com/VERIMAG-Polyhedra/VplTactic) plugin for Coq (also install `coq-vpl` and `vpl-core`)

      ```
      opam install coq-vpltactic
      ```

2. __From sources__

    1. _Dependencies_

        The VPL requires the following packages:

        * [ocaml](http://caml.inria.fr/ocaml/index.en.html)
        __required version >= 4.08.0__

        * [zarith](https://forge.ocamlcore.org/projects/zarith)
        _available in OPAM_
        __tested with version 1.4.1 and 1.9.1__

        * [dune](https://dune.readthedocs.io/en/stable/)
        _available in OPAM_
        __required version >= 2.1__

    2. _Compiling the VPL_

       (Optional) To re-extract from the coq files, simply run at the root directory

            make coq_extract

       To compile the VPL, simply run from the root directory

            make vpl

       Tests can be run by typing

            make check

       Finally, to install the library with ocamlfind, type

            make install

       To uninstall the library from ocamlfind, run

            make uninstall

## Documentation
Documentation can be built from sources with

    make doc; make open_doc

It contains the interface of main modules.

## Using the VPL

There are several ways to use the library.

1. From Coq
(see opam package `coq-vpl`)

2. As a Coq tactic
(see [VplTactic](https://github.com/VERIMAG-Polyhedra/VplTactic))

3. As an OCaml library
(see opam package `vpl-core`)

As an OCaml library, the entry point of the VPL is the module UserInterface, which contains several version of the abstract domain.
Polyhedral domains can work over Q or Z, and there are three levels of certification, which gives 6 possible domains.
The level of certifications are:

* _No certification_: no certificate is produced. This is implemented in modules _UncertifiedZ_ and _UncertifiedQ_.

* _OCaml certification_: Each operator of the domain produces a _certificate_, ie a witness of its computation that can be checked. This is implemented in modules _OCamlCertifiedZ_ and _OCamlCertifiedQ_.  

3. As an OCaml library
(see opam package `vpl-core`)

* _Coq certification_: In addition to guarantees offered by OCaml certification, certificates are here extracted from Coq proven types. This is implemented in modules _CoqCertifiedZ_ and _CoqCertifiedQ_.  

## Documentation

1. Online: [link](https://amarechal.gitlab.io/home/projects/vpl/vpl-core/)

2. From sources: `make doc`

The html documentation is then generated in the folder `_build/_doc/_html/index.html`
