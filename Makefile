find:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -linkpkg -o draw.byte draw.ml
	js_of_ocaml draw.byte

build:
	ocamlbuild -tag debug -use-ocamlfind -package js_of_ocaml -package js_of_ocaml-ppx draw.byte

compile:
	js_of_ocaml --pretty --debug-info --source-map draw.byte

build-old:
	ocamlbuild -use-ocamlfind \
		-plugin-tag "package(js_of_ocaml.ocamlbuild)" \
		-no-links \
		draw.js

clean:
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

physics:
	ocamlbuild -use-ocamlfind test_physics.byte && ./test_physics.byte

8b:
	ocamlbuild -use-ocamlfind test_eightball.byte && ./test_eightball.byte
demo: build compile

# # clean:
# # 	ocamlbuild -clean
# # 	rm -f a4src.zip
# # ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -no-links draw.byte
