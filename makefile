all: miniml

miniml: expr.ml evaluation.ml miniml.ml
	ocamlbuild miniml.byte

test: tests.ml
	ocamlbuild tests.byte

clean:
	rm -rf *.byte _build
