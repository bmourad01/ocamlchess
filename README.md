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
- ocaml_intrinsics
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

# Setup

- To build and install the project, run `make` (these can be done individually with `make build` and `make install`, respectively).
- To uninstall the project, run `make uninstall`.
- To clean the build artifacts, run `make clean`.
- To run the unit tests, run `make test`.
- For generating documentation, run `make doc`.

The generated documentation (in HTML format) should be located in `./chess/_build/default/_doc/_html/index.html`, which can be opened in your browser of choice.

# Usage

For a list of commands (and general info), run `ocamlchess --help`.

To communicate with the engine via UCI, run `ocamlchess uci`.
The engine will listen for commands on `stdin` and will respond on `stdout`.
See [this](http://wbec-ridderkerk.nl/html/UCIProtocol.html) page for a description of the UCI protocol.

To use the custom testing GUI, run `ocamlchess gui`.
See `ocamlchess gui --help` for more information.

`ocamlchess perft <n>` will run the *perf*ormance *t*est up to depth `n`.
See `ocamlchess perft --help` for more information.

# Acknowledgements

We would like to thank the following projects/resources, which have been used to aid the development of `ocamlchess`:

- The [Chess Programming Wiki](https://www.chessprogramming.org/)
- [Stockfish](https://github.com/official-stockfish/Stockfish)
- [Inanis](https://github.com/Tearth/Inanis)
- [mantissa](https://github.com/jtheardw/mantissa)
- [Shallow Blue](https://github.com/GunshipPenguin/shallow-blue)
- [wukongJS](https://github.com/maksimKorzh/wukongJS)
