%{
  open General.Abbr
  module Lexing = OCamlStandard.Lexing
  module Printf = OCamlStandard.Printf
  module Pervasives = OCamlStandard.Pervasives

  open Grammar
%}

%token <string> IDENTIFIER
%token <string> RULE
%token <string> TERMINAL
%token ALTERNATIVE
%token EOF
%token RANGE
%token BEGIN_GROUP, END_GROUP
%token BEGIN_OPTION, END_OPTION
%token BEGIN_REPEAT_ONE, END_REPEAT_ONE
%token BEGIN_REPEAT_ZERO, END_REPEAT_ZERO

%start grammar

%type <Grammar.t> grammar

%%

grammar:
  | rules=nonempty_list(rule) EOF
    { grammar rules }

rule:
  | name=RULE option(pair(RANGE, ALTERNATIVE)) definition=definition
    (* @todo Display that a rule is an addition to an existing rule in case there is a (RANGE, ALTERNATIVE) *)
    { rule name definition }

definition:
  | elements=separated_nonempty_list(ALTERNATIVE, range)
    { alternative elements }

range:
  | min_max=separated_pair(sequence, RANGE, sequence)
    { let (min, max) = min_max in range min max }
  | seq=sequence
    { seq }

sequence:
  | elements=nonempty_list(single_definition)
    { sequence elements }

single_definition:
  | value=TERMINAL
    { terminal value }
  | name=IDENTIFIER
    { non_terminal name }
  | definition=delimited(BEGIN_GROUP, definition, END_GROUP)
    { definition }
  | definition=delimited(BEGIN_OPTION, definition, END_OPTION)
    { alternative [null; definition] }
  | definition=delimited(BEGIN_REPEAT_ZERO, definition, END_REPEAT_ZERO)
    { repetition null definition }
  | definition=delimited(BEGIN_REPEAT_ONE, definition, END_REPEAT_ONE)
    { repetition definition null }
