open General.Abbr
module Arg = OCamlStandard.Arg
module Sys = OCamlStandard.Sys

module Arguments = struct
  let simplify = ref true

  let rule_label_font_size = ref Drawer.DefaultPrimarySettings.rule_label_font_size
  let space_between_rules = ref Drawer.DefaultPrimarySettings.space_between_rules
  let definitions_font_size = ref Drawer.DefaultPrimarySettings.definitions_font_size
  let line_width = ref Drawer.DefaultPrimarySettings.line_width

  let arrow_size = ref Drawer.DefaultSecondarySettings.arrow_size
  let dead_end_size = ref Drawer.DefaultSecondarySettings.dead_end_size
  let minimal_horizontal_spacing = ref Drawer.DefaultSecondarySettings.minimal_horizontal_spacing
  let minimal_vertical_spacing = ref Drawer.DefaultSecondarySettings.minimal_vertical_spacing
  let start_radius = ref Drawer.DefaultSecondarySettings.start_radius
  let stop_radius = ref Drawer.DefaultSecondarySettings.stop_radius
  let turn_radius = ref Drawer.DefaultSecondarySettings.turn_radius
  let ellipsis_size = ref Drawer.DefaultSecondarySettings.ellipsis_size

  let syntax = ref None
  let files = ref []

  let inlines = ref []

  let inline rule_name =
    inlines := rule_name::!inlines

  let spec = Arg.[
    (
      "--syntax",
      String (fun s -> syntax := Some s),
      (
        Frmt.apply
          "[%s]\n           Force the syntax of the input files, regardless of their extensions."
          (
            Parse.Syntax.all
            |> Li.map ~f:Parse.Syntax.to_string
            |> StrLi.join ~sep:"|"
          )
      )
    );
    ("--no-simplify", Clear simplify, " Don't merge common symbols before rails join or after they split");

    ("--inline", String inline, "STRING  Inline this rule's definition where it's used");
    (* @todo Ignore a rule *)
    (* @todo Draw only some rules *)
    (* @todo Draw each rule in a separate file *)
    (* @todo Choose output name *)

    ("--rule-label-font-size", Set_float rule_label_font_size, (Frmt.apply "FLOAT  Set rule label font size (default: %.02f)" !rule_label_font_size));
    ("--space-between-rules", Set_float space_between_rules, (Frmt.apply "FLOAT  Set space between rules (default: %.02f)" !space_between_rules));
    ("--definitions-font-size", Set_float definitions_font_size, (Frmt.apply "FLOAT  Set definitions font size (default: %.02f)" !definitions_font_size));
    ("--line-width", Set_float line_width, (Frmt.apply "FLOAT  Set line width (default: %.02f)" !line_width));

    ("--arrow-size", Set_float arrow_size, (Frmt.apply "FLOAT  Set arrow size (default: %.02f)" !arrow_size));
    ("--dead-end-size", Set_float dead_end_size, (Frmt.apply "FLOAT  Set dead end size (default: %.02f)" !dead_end_size));
    ("--minimal-horizontal-spacing", Set_float minimal_horizontal_spacing, (Frmt.apply "FLOAT  Set minimal horizontal spacing (default: %.02f)" !minimal_horizontal_spacing));
    ("--minimal-vertical-spacing", Set_float minimal_vertical_spacing, (Frmt.apply "FLOAT  Set minimal vertical spacing (default: %.02f)" !minimal_vertical_spacing));
    ("--start-radius", Set_float start_radius, (Frmt.apply "FLOAT  Set start radius (default: %.02f)" !start_radius));
    ("--stop-radius", Set_float stop_radius, (Frmt.apply "FLOAT  Set stop radius (default: %.02f)" !stop_radius));
    ("--turn-radius", Set_float turn_radius, (Frmt.apply "FLOAT  Set turn radius (default: %.02f)" !turn_radius));
    ("--ellipsis-size", Set_float ellipsis_size, (Frmt.apply "FLOAT  Set ellipsis size (default: %.02f)" !ellipsis_size));
  ]

  let usage =
    Frmt.apply
      "Draw railroad diagrams of a grammar expressed in EBNF\n\
      \n\
      %s [options] input_files\n\
      \n\
      Without the -syntax option, the syntax used is based on the file extension:\n\
      %s\
      \n\
      Options:\n\
      "
      Sys.argv.(0)
      (
        Parse.Syntax.all
        |> Li.map ~f:(fun syntax ->
          Frmt.apply
            "  - .%s for %s\n    (See %s)\n"
            (Parse.Syntax.to_string syntax)
            (Parse.Syntax.description syntax)
            (Parse.Syntax.online_reference syntax)
        )
        |> StrLi.join
      )

  let parse () =
    Arg.parse spec (fun f -> files := f::!files) usage

  let usage () =
    Arg.usage spec usage
end

let () = Arguments.parse ()

module Drawer = Drawer.Make(Cairo)(struct
  let rule_label_font_size = !Arguments.rule_label_font_size
  let space_between_rules = !Arguments.space_between_rules
  let definitions_font_size = !Arguments.definitions_font_size
  let line_width = !Arguments.line_width
end)(struct
  let arrow_size = !Arguments.arrow_size
  let dead_end_size = !Arguments.dead_end_size
  let minimal_vertical_spacing = !Arguments.minimal_vertical_spacing
  let minimal_horizontal_spacing = !Arguments.minimal_horizontal_spacing
  let start_radius = !Arguments.start_radius
  let stop_radius = !Arguments.stop_radius
  let turn_radius = !Arguments.turn_radius
  let ellipsis_size = !Arguments.ellipsis_size
end)

let () =
  let syntax =
    !Arguments.syntax
    |> Opt.map ~f:Parse.Syntax.of_string
  in
  match !Arguments.files with
    | [] -> Arguments.usage ()
    | files ->
      files
      |> Li.iter ~f:(fun input_name ->
        let output_name = Frmt.apply "%s.png" input_name in
        StdOut.print
          "Drawing %s%s to %s\n"
          input_name
          (syntax |> Opt.value_map ~def:"" ~f:(fun syntax -> Frmt.apply " (as %s)" (Parse.Syntax.to_string syntax)))
          output_name
        ;
        StdOut.flush ();
        let grammar = Parse.parse_file ?syntax input_name in
        let grammar =
          if !Arguments.simplify then
            Grammar.simplify grammar
          else
            grammar
        in
        let grammar =
          !Arguments.inlines
          |> Li.fold ~init:grammar ~f:Grammar.inline
        in
        let context = Cairo.create (Cairo.Image.create Cairo.Image.RGB24 ~width:1 ~height:1) in
        let (w, h) = Drawer.measure grammar ~context in
        let image = Cairo.Image.create Cairo.Image.RGB24 ~width:(Int.of_float w) ~height:(Int.of_float h) in
        let context = Cairo.create image in
        Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
        Cairo.paint context;
        Cairo.set_source_rgb context ~r:0. ~g:0. ~b:0.;
        Drawer.draw grammar ~context;
        Cairo.PNG.write image output_name
      )
