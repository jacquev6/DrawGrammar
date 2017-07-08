open General.Abbr

module Drawer = Drawer.Make(JsOfOCairo)

let draw_grammar_on_canvas syntax grammar canvas =
  let grammar = Grammar.parse ~syntax grammar in
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  let context = JsOfOCairo.create (canvas##getContext Dom_html._2d_) in
  let (w, h) = Drawer.measure grammar ~context in
  canvas##.width := Int.of_float w;
  canvas##.height := Int.of_float h;
  Drawer.draw grammar ~context

let () = Js.Unsafe.global##.draw_grammar_on_canvas_ := Js.wrap_callback draw_grammar_on_canvas
