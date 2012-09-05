.PHONY: all clean install build
all: build doc

NAME=xenstore
J=4

export OCAMLRUNPARAM=b

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

build-xen: setup.bin
	./setup.bin -configure --enable-xen --disable-unix
	./setup.bin -build -j $(J)

build-unix: setup.bin
	./setup.bin -configure --disable-xen --enable-unix --enable-tests
	./setup.bin -build -j $(J)

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install

# oasis bug?
#test: setup.bin build
#	@./setup.bin -test
test:
	_build/core_test/xs_test.native
	_build/server_test/server_test.native


reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
