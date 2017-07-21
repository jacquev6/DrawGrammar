#!/bin/bash

set -o errexit

for p in src/*_Parser.mly
do
  m=${p%_Parser.mly}_Messages.messages
  c=${p%_Parser.mly}_Messages.ml
  menhir --list-errors $p > $m.dummies
  if [ -f $m ]
  then
    menhir --compare-errors $m.dummies --compare-errors $m $p
    rm $m.dummies
  else
    mv $m.dummies $m
  fi
  menhir --compile-errors $m $p > $c
done

function build {
  ocamlbuild -use-ocamlfind -menhir "menhir --table" -no-links -tag debug -plugin-tag "package(js_of_ocaml.ocamlbuild)" $@
}

cd src
build unit_tests.byte
cd ..
src/_build/unit_tests.byte

cd src
build draw_grammar.byte
cd ..
src/_build/draw_grammar.byte --help
cd docs
[ -e python2.7.python-ebnf ] || wget https://raw.githubusercontent.com/python/cpython/2.7/Grammar/Grammar --output-document python2.7.python-ebnf
[ -e python3.6.python-ebnf ] || wget https://raw.githubusercontent.com/python/cpython/3.6/Grammar/Grammar --output-document python3.6.python-ebnf
../src/_build/draw_grammar.byte *.*-ebnf
mv *.*-ebnf.png ..
cd ..
echo
echo "Have a look at $(pwd)/*.png"
echo

cd src
build draw_grammar_js.js drawing_tests.js
cd ..
cp src/_build/draw_grammar_js.js docs
echo
echo "Have a look at $(pwd)/drawing_tests.html"
echo
echo "Have a look at $(pwd)/docs/index.html"
echo
