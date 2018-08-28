ACLOCAL_AMFLAGS = -I m4

mypkgdir=$(pkglibdir)

.DEFAULT_GOAL = all

MODULES = sliceS.ml mutSlice.ml slice.ml monoSlice.ml \
	mutCharSlice.ml charSlice.ml mutFloatSlice.ml floatSlice.ml

SUFFIXES = .mll .ml .mli .cmi .cmo .cmx .cmxa .cmxs .doc.ml

mypkg_SCRIPTS = META $(MODULES:.ml=.cmi)

if OCAML_SUPPORTS_BYTECODE
mypkg_SCRIPTS += slices.cma
endif

if OCAML_SUPPORTS_NATIVE
mypkg_SCRIPTS += slices.cmxa slices.a $(MODULES:.ml=.cmx)
endif

if OCAML_SUPPORTS_SHARED
mypkg_SCRIPTS += slices.cmxs
endif

OCAMLFLAGS = -package stdcompat

OCAMLCFLAGS = $(OCAMLFLAGS)

OCAMLLDFLAGS = $(OCAMLFLAGS)

.mli.cmi :
	$(OCAMLBEST) -c $(OCAMLCFLAGS) $<

.ml.cmo :
	$(OCAMLC) -c $(OCAMLCFLAGS) $<

.ml.cmx :
	$(OCAMLOPT) -c $(OCAMLCFLAGS) $<

.mli.doc.ml :
	$(OCAMLCODOC) $<

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES:.ml=.mli)
	$(OCAMLDEP) $^ >$@ || rm -f .depend

slices.cma : $(MODULES:.ml=.cmo)
	$(OCAMLC) $(OCAMLLDFLAGS) -a $^ -o $@

slices.cmxa : $(MODULES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -a $^ -o $@

slices.cmxs : $(MODULES:.ml=.cmx)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -shared $^ -o $@

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.o *.a

.PHONY : tests

tests : slice_tests
	./slice_tests

sliceS.doc.$(CMOX) : slices.$(CMAX)

slice_tests : slices.$(CMAX) sliceS.doc.$(CMOX)
	$(OCAMLBEST) -linkpkg $(OCAMLLDFLAGS) $^ -o $@

doc : $(MODULES:.ml=.mli)
	mkdir -p doc
	$(OCAMLDOC) $(OCAMLFLAGS) -html -d $@ $^
	touch doc
