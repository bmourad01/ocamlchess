# Overview

This project aims to create an efficient, UCI-compatible chess engine in OCaml that is *functional first*.

# Requirements

For best performance, please ensure that a Flambda-enabled OCaml compiler is installed (minimum 4.09).

The following OCaml packages are required, available via `opam install <pkg>`:

- cmdliner
- core
- dune
- monads
- ounit2
- ppx_compare
- ppx_fields_conv
- ppx_hash
- ppx_sexp_conv

The custom testing GUI requires the following:

- A C++11-compatible compiler
- SFML 2.5

Finally, a version of Make (such as GNU Make) is needed for building the project.

# Instructions

- To build and install the project, run `make` (these can be done individually with `make build` and `make install`, respectively).
- To uninstall the project, run `make uninstall`.
- To clean the build artifacts, run `make clean`.
- To run the unit tests, run `make test`.
