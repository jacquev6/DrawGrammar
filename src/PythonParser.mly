%{
  open General.Abbr
  module Lexing = OCamlStandard.Lexing
  module Printf = OCamlStandard.Printf
  module Pervasives = OCamlStandard.Pervasives

  open Grammar
  open Grammar.Rule
  open Grammar.Definition
%}

%token <string> IDENTIFIER
%token <string> RULE
%token <string> TERMINAL
%token ALTERNATIVE
%token EOF
%token REPEAT_ONE
%token REPEAT_ZERO
%token START_GROUP, END_GROUP
%token START_OPTION, END_OPTION

%start syntax

%type <Grammar.t> syntax

%%

syntax:
  | rules=nonempty_list(rule) EOF
    { {rules} }

rule:
  | name=RULE definition=definition
    { {name; definition} }

definition:
  | alternative=alternative
    { alternative }

alternative:
  | elements=separated_nonempty_list(ALTERNATIVE, sequence)
    { Alternative {Alternative.elements}}

sequence:
  | elements=nonempty_list(repetition)
    { Sequence {Sequence.elements}}

repetition:
  | definition=single_definition REPEAT_ZERO
    { Repetition {Repetition.forward=Null; backward=definition} }
  | definition=single_definition REPEAT_ONE
    { Repetition {Repetition.forward=definition; backward=Null} }
  | definition=single_definition
    { definition }

single_definition:
  | value=TERMINAL
    { Terminal {Terminal.value }}
  | name=IDENTIFIER
    { NonTerminal {NonTerminal.name }}
  | definition=delimited(START_GROUP, definition, END_GROUP)
    { definition }
  | definition=delimited(START_OPTION, definition, END_OPTION)
    { Alternative {Alternative.elements=[Null; definition]} }
