.PHONY: clean install uninstall test

all:
	dune build --profile=release

clean:
	dune clean

install:
	dune install

uninstall:
	dune uninstall

test:
	dune test

doc:
	dune build @doc
