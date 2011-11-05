
OCAMLC=ocamlfind ocamlc -syntax camlp4o -package "lwt.syntax,bitstring.syntax,bitstring"

test: xs_packet.cmi xs_packet.cmo xb_stubs.o
	$(OCAMLC) -linkpkg -o test xs_packet.cmo xb_stubs.o

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<
