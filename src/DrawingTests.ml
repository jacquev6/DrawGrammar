(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let single_rule_grammars =
  Grammar.[
    ("terminal", Definition.Terminal {Terminal.value="in a rounded rectangle"});
    ("rule with a name longer than its definition", Definition.Terminal {Terminal.value="short"});
    ("non-terminal", Definition.NonTerminal {NonTerminal.name="in a rectangle"});
    ("special", Definition.Special {Special.value="in a pointy rectangle"});
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
    ("alternative with null branch", Definition.Alternative {Alternative.elements=[
      Definition.Null;
      Definition.Terminal {Terminal.value="t"};
    ]});
    ("sequence with null", Definition.Sequence {Sequence.elements=[
      Definition.Terminal {Terminal.value="same space here ->"};
      Definition.Null;
      Definition.Terminal {Terminal.value="and here ->"};
      Definition.Terminal {Terminal.value="t"};
    ]});
    ("repetition with long forward branch", Definition.Repetition {
      Repetition.forward = Definition.Terminal {Terminal.value="long branch"};
      backward = Definition.Terminal {Terminal.value="short"};
    });
    ("repetition with long backward branch", Definition.Repetition {
      Repetition.forward = Definition.Terminal {Terminal.value="short"};
      backward = Definition.Terminal {Terminal.value="long branch"};
    });
    ("repetition with null forward branch", Definition.Repetition {
      Repetition.forward = Definition.Null;
      backward = Definition.Terminal {Terminal.value="t"};
    });
    ("repetition with null backward branch", Definition.Repetition {
      Repetition.forward = Definition.Terminal {Terminal.value="t"};
      backward = Definition.Null;
    });
    ("nested alternatives", Definition.Alternative {Alternative.elements=[
      Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="t1"};
        Definition.Terminal {Terminal.value="t2"};
      ]};
      Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="t3"};
        Definition.Terminal {Terminal.value="t4"};
      ]};
    ]});
    ("nested repetitions", Definition.Repetition {
      Repetition.forward = Definition.Repetition {
        Repetition.forward = Definition.Terminal {Terminal.value="forward 1"};
        backward = Definition.Terminal {Terminal.value="backward 1"};
      };
      backward = Definition.Repetition {
        Repetition.forward = Definition.Terminal {Terminal.value="forward 2"};
        backward = Definition.Terminal {Terminal.value="backward 2"};
      };
    });
    ("alternatives in repetition", Definition.Repetition {
      Repetition.forward = Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="t1"};
        Definition.Terminal {Terminal.value="t2"};
      ]};
      backward = Definition.Alternative {Alternative.elements=[
        Definition.Terminal {Terminal.value="t3"};
        Definition.Terminal {Terminal.value="t4"};
      ]};
    });
    ("repetitions in alternative", Definition.Alternative {Alternative.elements=[
      Definition.Repetition {
        Repetition.forward = Definition.Terminal {Terminal.value="forward 1"};
        backward = Definition.Terminal {Terminal.value="backward 1"};
      };
      Definition.Repetition {
        Repetition.forward = Definition.Terminal {Terminal.value="forward 2"};
        backward = Definition.Terminal {Terminal.value="backward 2"};
      };
    ]});
    ("exception with long base", Definition.Except {
      Except.base = Definition.Terminal {Terminal.value="long base branch"};
      except = Definition.Terminal {Terminal.value="except"};
    });
    ("exception with long except", Definition.Except {
      Except.base = Definition.Terminal {Terminal.value="base"};
      except = Definition.Terminal {Terminal.value="long except branch"};
    });
  ]
  |> Li.map ~f:(fun (name, definition) ->
    (name, {Grammar.rules=[{Grammar.Rule.name; definition}]})
  )

let tests =
  single_rule_grammars
