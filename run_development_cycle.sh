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
[ -e lex.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/lex.etex --output-document lex.ocaml-etex-ebnf
[ -e names.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/names.etex --output-document names.ocaml-etex-ebnf
[ -e types.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/types.etex --output-document types.ocaml-etex-ebnf
[ -e const.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/const.etex --output-document const.ocaml-etex-ebnf
[ -e patterns.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/patterns.etex --output-document patterns.ocaml-etex-ebnf
[ -e expr.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/expr.etex --output-document expr.ocaml-etex-ebnf
[ -e typedecl.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/typedecl.etex --output-document typedecl.ocaml-etex-ebnf
[ -e classes.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/classes.etex --output-document classes.ocaml-etex-ebnf
[ -e modtypes.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/modtypes.etex --output-document modtypes.ocaml-etex-ebnf
[ -e modules.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/modules.etex --output-document modules.ocaml-etex-ebnf
[ -e compunit.ocaml-etex-ebnf ] || wget https://raw.githubusercontent.com/ocaml/ocaml/trunk/manual/manual/refman/compunit.etex --output-document compunit.ocaml-etex-ebnf
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
