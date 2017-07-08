(* Copyright 2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr

module Drawer = Drawer.Make(JsOfOCairo)


let drawing_tests =
  DrawingTests.tests
  |> Li.map ~f:(fun (name, grammar) ->
    object%js (_)
      val name = Js.string name
      method draw_on_canvas_ (canvas: Dom_html.element Js.t) = 
        let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
        let context = JsOfOCairo.create (canvas##getContext Dom_html._2d_) in
        let (w, h) = Drawer.measure grammar ~context in
        canvas##.width := Int.of_float w;
        canvas##.height := Int.of_float h;
        Drawer.draw grammar ~context
    end
  )
  |> Li.to_array

let () = Js.export "drawing_tests" drawing_tests
