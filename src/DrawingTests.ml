(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

let single_rule_grammars =
  Grammar.[
    ("terminal", terminal "in a rounded rectangle");
    ("rule with a name longer than its definition", terminal "short");
    ("non-terminal", non_terminal "in a rectangle");
    ("special", special "in an octogon");
    ("sequence", sequence [terminal "t1"; non_terminal "nt"; terminal "t2"]);
    ("alternative", alternative [terminal "short"; non_terminal "longestttttt"; terminal "medium"]);
    ("alternative with null branch", alternative [null; terminal "t"]);
    ("sequence with null", sequence [terminal "a"; null; terminal "b"; terminal "c"]);
    ("repetition with long forward branch", repetition (terminal "long branch") (terminal "short"));
    ("repetition with long backward branch", repetition (terminal "short") (terminal "long branch"));
    ("repetition with null forward branch", repetition null (terminal "t"));
    ("repetition with null backward branch", repetition (terminal "t") null);
    ("nested alternatives", alternative [
      alternative [terminal "t1"; terminal "t2"];
      alternative [terminal "t3"; terminal "t4"];
    ]);
    ("nested sequences", sequence [
      sequence [terminal "t1"; terminal "t2"];
      sequence [terminal "t3"; terminal "t4"];
    ]);
    ("nested repetitions", repetition
      (repetition (terminal "forward 1") (terminal "backward 1"))
      (repetition (terminal "forward 2") (terminal "backward 2"))
    );
    ("repetitions in sequence", sequence [
      repetition (terminal "forward 1") (terminal "backward 1");
      repetition (terminal "forward 2") (terminal "backward 2");
    ]);
    ("exceptions in sequence", sequence [
      except (terminal "base 1") (terminal "except 1");
      except (terminal "base 2") (terminal "except 2");
    ]);
    ("alternatives in sequence", sequence [
      alternative [terminal "t1"; terminal "t2"];
      alternative [terminal "t3"; terminal "t4"];
    ]);
    ("alternatives in repetition", repetition
      (alternative [terminal "t1"; terminal "t2"])
      (alternative [terminal "t3"; terminal "t4"])
    );
    ("repetitions in alternative", alternative [
      repetition (terminal "forward 1") (terminal "backward 1");
      repetition (terminal "forward 2") (terminal "backward 2");
    ]);
    ("exception with long base", except (terminal "long base branch") (terminal "except"));
    ("exception with long except", except (terminal "base") (terminal "long except branch"));
  ]
  |> Li.map ~f:(fun (name, definition) ->
    (name, Grammar.(grammar [rule name definition]))
  )

let tests =
  single_rule_grammars @ Grammar.[
    (
      "several rules",
      grammar [
        rule "rule1" (terminal "t1");
        rule "rule2" (terminal "t2");
      ]
    );
  ]
