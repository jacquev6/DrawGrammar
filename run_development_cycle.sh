#!/bin/bash

set -o errexit

cd src
ocamlbuild -use-ocamlfind -no-links -plugin-tag "package(js_of_ocaml.ocamlbuild)" -pkgs General draw_syntax.js unit_tests.js
cd ..

node src/_build/unit_tests.js

echo "Have a look at $(pwd)/demo.html"
