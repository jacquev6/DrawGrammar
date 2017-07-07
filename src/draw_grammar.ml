open General.Abbr

module Drawer = Drawer.Make(CairoContext)

let () =
  OCamlStandard.Sys.argv
  |> Ar.to_list
  |> Li.tail
  |> Li.iter ~f:(fun input_name ->
    let parts = Str.split input_name ~sep:"." in
    match parts with
      | [name; syntax] ->
        let output_name = OCamlStandard.Printf.sprintf "%s.png" name in
        OCamlStandard.Printf.printf "Drawing %s to %s\n" input_name output_name;
        let grammar = Grammar.parse ~name ~syntax "@todo Read input file" in
        let context = Cairo.create (Cairo.Image.create Cairo.Image.RGB24 ~width:1 ~height:1) in
        let (width, height) = Drawer.measure grammar ~context in
        let image = Cairo.Image.create Cairo.Image.RGB24 ~width ~height in
        let context = Cairo.create image in
        Cairo.set_source_rgb context ~r:1. ~g:1. ~b:1.;
        Cairo.paint context;
        Cairo.set_source_rgb context ~r:0. ~g:0. ~b:0.;
        Drawer.draw grammar ~context;
        Cairo.PNG.write image output_name
      | _ ->
        failwith "File name doesn't have exaclty one dot"
  )
