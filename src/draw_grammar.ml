open General.Abbr

(* @todo Parse command-line options for Drawer.Settings *)

module Drawer = Drawer.Make(Cairo)(Drawer.DefaultPrimarySettings)(Drawer.DefaultSecondarySettings)

let () =
  OCamlStandard.Sys.argv
  |> Ar.to_list
  |> Li.tail
  |> Li.iter ~f:(fun input_name ->
    let output_name = OCamlStandard.Printf.sprintf "%s.png" input_name in
    OCamlStandard.Printf.printf "Drawing %s to %s\n" input_name output_name;
    let grammar =
      input_name
      |> Parse.parse_file
      |> Grammar.normalize
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
