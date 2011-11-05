
OCAMLC=ocamlfind ocamlc -syntax camlp4o -package "lwt.syntax,bitstring.syntax,bitstring"

test: xs_packet.cmi xs_packet.cmo
	$(OCAMLC) -linkpkg -o test xs_packet.cmo

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

.PHONY:clean
clean:
	rm -f *.cmo *.cmi *.cmx test *.o *.a