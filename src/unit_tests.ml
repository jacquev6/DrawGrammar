open General.Abbr
open Tst

let test =
  "DrawGrammar unit tests" >:: [
    Grammar.UnitTests.test;
    Parse.UnitTests.test;
  ]

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv test)
