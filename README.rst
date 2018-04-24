*DrawGrammar* is a tool to draw `railroad diagrams <https://en.wikipedia.org/wiki/Syntax_diagram>`_ of an `EBNF <https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form>`_ grammar.
It is available as an `interactive demo <http://jacquev6.github.io/DrawGrammar/>`_ and as a command-line tool.

It's licensed under the `MIT license <http://choosealicense.com/licenses/mit/>`_.
It's available on `OPAM <https://opam.ocaml.org/packages/DrawGrammar/>`_.
Its `source code <https://github.com/jacquev6/DrawGrammar>`_ is on GitHub.

Questions? Remarks? Bugs? Want to contribute? `Open an issue <https://github.com/jacquev6/DrawGrammar/issues>`_!

.. image:: https://img.shields.io/travis/jacquev6/DrawGrammar/master.svg
    :target: https://travis-ci.org/jacquev6/DrawGrammar

.. image:: https://img.shields.io/github/issues/jacquev6/DrawGrammar.svg
    :target: https://github.com/jacquev6/DrawGrammar/issues

.. image:: https://img.shields.io/github/forks/jacquev6/DrawGrammar.svg
    :target: https://github.com/jacquev6/DrawGrammar/network

.. image:: https://img.shields.io/github/stars/jacquev6/DrawGrammar.svg
    :target: https://github.com/jacquev6/DrawGrammar/stargazers

Quick start
===========

The simplest way is to use the `interactive demo <http://jacquev6.github.io/DrawGrammar/>`_, load examples and play with the settings.

If you want to use it locally as a command-line tool, install it from OPAM::

    $ opam install DrawGrammar

Then create a ``my_grammar.iso-ebnf`` file containing, for example::

    expr = term, { ( '+' | '-' ) , term };

    term = { factor, ( '*' | '/' ) }, factor;

    factor = integer | '(', expr, ')';

    integer = [ '-' ], digit, { digit };

    digit = '0' | '1' | '...' | '9';

And draw it::

    $ draw_grammar my_grammar.iso-ebnf

If you want to tweak settings, the help is accessible using::

    $ draw_grammar --help

Development version
===================

If you want to use the development version on the command line, you can::

    $ opam pin add General --dev-repo
    $ opam pin add DrawGrammar --dev-repo

This always matches the version used in the interactive demo.
