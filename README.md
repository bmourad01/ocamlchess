# Overview

This project aims to create an efficient, UCI-compatible chess engine in OCaml that is *functional first*.

# Requirements

For best performance, please ensure that a Flambda-enabled OCaml compiler is installed (minimum 4.09).

The following OCaml packages are required, available via `opam install <pkg>`:

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
