open General.Abbr

module Drawer = Drawer.Make(CanvasContext)

let draw_grammar_on_canvas syntax grammar canvas =
  let grammar = Grammar.parse ~syntax grammar in
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  let context = canvas##getContext (Dom_html._2d_) in
  let (width, height) = Drawer.measure grammar ~context in
  canvas##width <- width;
  canvas##height <- height;
  Drawer.draw grammar ~context

let () = Js.Unsafe.global##draw_grammar_on_canvas_ (* why do I need a last undesrcore? *) <- Js.wrap_callback draw_grammar_on_canvas
