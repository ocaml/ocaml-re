.PHONY: all clean install build
all: build

setup.data:
	ocaml setup.ml -configure

build: setup.data
	ocaml setup.ml -build

install:
	ocaml setup.ml -install

reinstall:
	ocamlfind -remove regexp || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log
