open General.Abbr
open Tst

let () =
  "DrawGrammar unit tests" >::: [
    Grammar.UnitTests.test;
    Parse.UnitTests.test;
  ]
  |> run
  |> report_to_console
