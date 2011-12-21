
NAME = re

OCAMLC   = ocamlfind ocamlc -g
OCAMLOPT = ocamlfind ocamlopt -unsafe
OCAMLDEP = ocamldep

INCFLAGS =
OBJECTS  = cset.cmo automata.cmo \
	   re.cmo re_posix.cmo re_emacs.cmo re_perl.cmo re_glob.cmo re_str.cmo
XOBJECTS = $(OBJECTS:cmo=cmx)
INTFS = re.mli re_posix.mli re_emacs.mli re_perl.mli re_glob.mli re_str.mli

ARCHIVE  = $(NAME).cma
XARCHIVE = $(NAME).cmxa

REQUIRES =
PREDICATES =

all: $(ARCHIVE)
opt: $(XARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) -package "$(REQUIRES)" -linkpkg \
	          -predicates "$(PREDICATES)" $(OBJECTS)
$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) -package "$(REQUIRES)" -linkpkg \
	          -predicates "$(PREDICATES)" $(XOBJECTS)

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(INCFLAGS) -c $<
.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(INCFLAGS) -c $<
.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" -predicates "$(PREDICATES)" \
	          $(INCFLAGS) -c $<

depend: *.ml *.mli
	$(OCAMLDEP) $(INCFLAGS) *.ml *.mli util/*.ml util/*.mli > depend
include depend

install: all
	{ test ! -f $(XARCHIVE) || extra="$(XARCHIVE) "`basename $(XARCHIVE) .cmxa`.a; }; \
	ocamlfind install $(NAME) $(INTFS) $(INTFS:mli=cmi) $(ARCHIVE) META $$extra

uninstall:
	ocamlfind remove $(NAME)

clean::
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o
	rm -f util/*.cmi util/*.cmo util/*.cmx util/*.o

clean::
	cd tests; make clean

realclean: clean
	rm -f *~ util/*~

distrib: realclean
	cd ..; tar zcvf re.tar.gz --exclude CVS re

check: $(ARCHIVE)
	fort $(ARCHIVE) -env tests/env.ml \
	tests/test_re.ml tests/test_emacs.ml tests/test_perl.ml
