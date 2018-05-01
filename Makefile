test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

physics:
	ocamlbuild -use-ocamlfind test_physics.byte && ./test_physics.byte

# # clean:
# # 	ocamlbuild -clean
# # 	rm -f a4src.zip
