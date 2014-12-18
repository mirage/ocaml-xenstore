.PHONY: all clean install build
all: build doc

NAME=xenstore
J=4

include config.mk
config.mk:
	echo Please re-run configure
	exit 1

export OCAMLRUNPARAM=b

TESTS ?= --enable-tests

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	@./setup.bin -configure $(TESTS)

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
	_build/core_test/core_test.native


reinstall: setup.bin
	@ocamlfind remove $(NAME) || true
	@./setup.bin -reinstall

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
