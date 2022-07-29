# Overview

This project aims to create an efficient, UCI-compatible chess engine (and library) in OCaml.

# Requirements

For best performance, please ensure that a Flambda-enabled OCaml compiler is installed.

The following OCaml packages are required, available via `opam install <pkg>`:

- bap-future
- cmdliner
- core_kernel
- dune
- monads
- odoc
- ounit2
- ppx_compare
- ppx_fields_conv
- ppx_hash
- ppx_sexp_conv

The custom testing GUI requires the following:

- CSFML 2.5

For building, a version of Make (such as GNU Make) is needed for building the project.

Note that this project assumes that the target system is POSIX-compliant (Windows users may encounter some trouble).

# Instructions

- To build and install the project, run `make` (these can be done individually with `make build` and `make install`, respectively).
- To uninstall the project, run `make uninstall`.
- To clean the build artifacts, run `make clean`.
- To run the unit tests, run `make test`.
- For generating documentation, run `make doc`.

The generated documentation (in HTML format) should be located in `./chess/_build/default/_doc/_html/index.html`, which can be opened in your browser of choice.
