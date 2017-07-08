(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let single_rule_grammars =
  Grammar.[
    ("terminal", Definition.Terminal {Terminal.value="in a rounded rectangle"});
    ("rule with a name longer than its definition", Definition.Terminal {Terminal.value="short"});
    ("non-terminal", Definition.NonTerminal {NonTerminal.name="in a rectangle"});
    ("sequence", Definition.Sequence {Sequence.elements=[
        Definition.Terminal {Terminal.value="t1"};
        Definition.NonTerminal {NonTerminal.name="nt"};
        Definition.Terminal {Terminal.value="t2"};
    ]});
    ("alternative", Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="short"};
        Definition.NonTerminal {NonTerminal.name="longestttttt"};
        Definition.Terminal {Terminal.value="medium"};
    ]});
    ("alternative with one branch", Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="lone"};
    ]});
    ("repetition with long forward branch", Definition.Repetition {
        Repetition.
        forward = Definition.Terminal {Terminal.value="long branch"};
        backward = Definition.Terminal {Terminal.value="short"};
    });
    ("repetition with long backward branch", Definition.Repetition {
        Repetition.
        forward = Definition.Terminal {Terminal.value="short"};
        backward = Definition.Terminal {Terminal.value="long branch"};
    });
  ]
  |> Li.map ~f:(fun (name, definition) ->
    (name, {Grammar.rules=[{Grammar.Rule.name; definition}]})
  )

let tests =
  single_rule_grammars
