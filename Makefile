all: phc

phc:
	ocamlbuild -pkg unix -I src src/phc.native --

test:
	ocamlbuild -pkg oUnit -I src tests/unit_tests.native --

clean:
	ocamlbuild -clean
