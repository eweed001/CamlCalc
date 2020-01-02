UNITS=arithmetic main statistics matrix trigonometric myset fibonacci authors breakthecode monty imports
MLS_WITHOUT_MLIS=ast
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir
PKGS=oUnit

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private calculator.zip _build
	
calc:
	$(OCAMLBUILD) calc.byte && ./calc.byte

zip:
	zip calculator.zip *

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
