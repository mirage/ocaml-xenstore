
OCAMLC=ocamlfind ocamlc -syntax camlp4o -package "oUnit,lwt.unix,lwt.syntax,bitstring.syntax,bitstring"

all: unittest xs

.PHONY: test
test: unittest
	./unittest

unittest: xs_packet.cmi xs_packet.cmo xs_packet_test.cmo
	$(OCAMLC) -linkpkg -o unittest xs_packet.cmo xs_packet_test.cmo

xs: xs_packet.cmi xs_packet.cmo xs_client.cmo xs_transport_unix.cmo xs_client_cli.cmo
	$(OCAMLC) -linkpkg -o client xs_packet.cmo xs_client.cmo xs_transport_unix.cmo xs_client_cli.cmo

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

.PHONY:clean
clean:
	rm -f *.cmo *.cmi *.cmx unittest xs *.o *.a