#!/bin/bash

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
	-g hz_model.ml test.ml

if [ $? = 0 ]; then
  ./test 
else
	echo "Test Build Fail"
fi
            

