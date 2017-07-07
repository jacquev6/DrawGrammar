open General.Abbr

module Drawer = Drawer.Make(Cairo)

let single_rule_grammars =
    Grammar.[
        ("terminal", Definition.Terminal {Terminal.value="in a rounded rectangle"})
    ]
    |> Li.map ~f:(fun (name, definition) ->
        {
            Grammar.name = Some name;
            rules = [{Grammar.Rule.name; definition}];
        }
    )

let grammars =
    single_rule_grammars

let () =
    grammars
    |> Li.iter ~f:(fun ({Grammar.name; _} as grammar) ->
        let output_name = OCamlStandard.Printf.sprintf "drawing_tests/%s.png" (Opt.value name) in
        let context = Cairo.create (Cairo.Image.create Cairo.Image.RGB24 ~width:1 ~height:1) in
        Cairo.scale context ~x:3. ~y:3.;
        let (width, height) = Drawer.measure grammar ~context in
        let image = Cairo.Image.create Cairo.Image.RGB24 ~width:(3 * (width + 10)) ~height:(3 * (height + 10)) in
        let context = Cairo.create image in
        Cairo.scale context ~x:3. ~y:3.;
        Cairo.set_source_rgb context ~r:0.8 ~g:0.8 ~b:0.8;
        Cairo.paint context;
        Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
        Cairo.translate context ~x:5. ~y:5.;
        Cairo.rectangle context ~x:0. ~y:0. ~w:(Fl.of_int width) ~h:(Fl.of_int height);
        Cairo.fill context;
        Cairo.set_source_rgb context ~r:0. ~g:0. ~b:0.;
        Drawer.draw grammar ~context;
        Cairo.PNG.write image output_name
    )
