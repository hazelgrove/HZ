ocamlfind ocamlc \
	-o test \
	-package oUnit \
	-package js_of_ocaml \
	-package tyxml \
	-package js_of_ocaml.tyxml \
	-package js_of_ocaml.syntax \
	-package lwt.ppx \
	-package js_of_ocaml.ppx \
	-package react \
	-package reactiveData \
	-linkpkg \
	-g hz.ml test.ml

./test 