#!/bin/bash

set -o errexit

cd src
ocamlbuild -use-ocamlfind -no-links -plugin-tag "package(js_of_ocaml.ocamlbuild)" -syntax camlp4o -pkgs General,js_of_ocaml,js_of_ocaml.syntax draw_grammar.js unit_tests.js
cd ..

src/_build/unit_tests.byte

nodejs src/_build/unit_tests.js

echo "Have a look at $(pwd)/demo.html"
