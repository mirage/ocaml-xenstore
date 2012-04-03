
OBJS = define \
	stdext \
	trie \
	config \
	logging \
	quota \
	perms \
	symbol \
	utils \
	store \
	disk \
	transaction \
	event \
	domain \
	domains \
	connection \
	connections \
	parse_arg \
	process \
	xenstored

INTF = symbol.cmi trie.cmi
XENSTOREDLIBS = \
	unix.cmxa

PROGRAMS = oxenstored

OCAML_PROGRAM = oxenstored

all: $(INTF) $(PROGRAMS)

bins: $(PROGRAMS)

install: all
	$(INSTALL_DIR) $(DESTDIR)$(SBINDIR)
	$(INSTALL_PROG) oxenstored $(DESTDIR)$(SBINDIR)
	$(INSTALL_DIR) $(DESTDIR)$(XEN_CONFIG_DIR)
	$(INSTALL_DATA) oxenstored.conf $(DESTDIR)$(XEN_CONFIG_DIR)

OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt

oxenstored: $(OBJS:=.cmx)
	$(OCAMLOPT) -linkpkg -o oxenstored $<


%.cmi: %.mli
	$(OCAMLC) -c -o $@ $<

%.cmo: %.ml %.cmi
	$(OCAMLC) -c -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<
