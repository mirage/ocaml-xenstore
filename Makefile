
OCAMLC=ocamlfind ocamlc -syntax camlp4o -package "oUnit,lwt.unix,lwt.syntax,bitstring.syntax,bitstring"

all: test client

test: xs_packet.cmi xs_packet.cmo xs_packet_test.cmo
	$(OCAMLC) -linkpkg -o test xs_packet.cmo xs_packet_test.cmo

client: xs_packet.cmi xs_packet.cmo xs_client.cmo
	$(OCAMLC) -linkpkg -o client xs_packet.cmo xs_client.cmo

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

.PHONY:clean
clean:
	rm -f *.cmo *.cmi *.cmx test *.o *.a