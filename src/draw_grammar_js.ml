open General.Abbr

let parse_grammar syntax grammar =
  let syntax =
    syntax
    |> Js.to_string
    |> Parse.Syntax.of_string
  in
  grammar
  |> Js.to_string
  |> Parse.parse_string ~syntax
  |> Grammar.simplify

class type settings = object
  method arrow_size_: float Js.optdef Js.readonly_prop
  method dead_end_size_: float Js.optdef Js.readonly_prop
  method definitions_font_size_: float Js.optdef Js.readonly_prop
  method line_width_: float Js.optdef Js.readonly_prop
  method minimal_vertical_spacing_: float Js.optdef Js.readonly_prop
  method minimal_horizontal_spacing_: float Js.optdef Js.readonly_prop
  method rule_label_font_size_: float Js.optdef Js.readonly_prop
  method space_between_rules_: float Js.optdef Js.readonly_prop
  method start_radius_: float Js.optdef Js.readonly_prop
  method stop_radius_: float Js.optdef Js.readonly_prop
  method turn_radius_: float Js.optdef Js.readonly_prop
end

let draw grammar (canvas: Dom_html.element Js.t) (settings: settings Js.t) =
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  let context = JsOfOCairo.create (canvas##getContext Dom_html._2d_) in
  let module Drawer = Drawer.Make(JsOfOCairo)(struct
    let arrow_size = Js.Optdef.to_option settings##.arrow_size_
    let dead_end_size = Js.Optdef.to_option settings##.dead_end_size_
    let definitions_font_size = Js.Optdef.to_option settings##.definitions_font_size_
    let line_width = Js.Optdef.to_option settings##.line_width_
    let minimal_vertical_spacing = Js.Optdef.to_option settings##.minimal_vertical_spacing_
    let minimal_horizontal_spacing = Js.Optdef.to_option settings##.minimal_horizontal_spacing_
    let rule_label_font_size = Js.Optdef.to_option settings##.rule_label_font_size_
    let space_between_rules = Js.Optdef.to_option settings##.space_between_rules_
    let start_radius = Js.Optdef.to_option settings##.start_radius_
    let stop_radius = Js.Optdef.to_option settings##.stop_radius_
    let turn_radius = Js.Optdef.to_option settings##.turn_radius_
  end) in
  let (w, h) = Drawer.measure grammar ~context in
  canvas##.width := Int.of_float w;
  canvas##.height := Int.of_float h;
  Drawer.draw grammar ~context

let draw_grammar_on_canvas syntax grammar canvas settings =
  let grammar = parse_grammar syntax grammar in
  draw grammar canvas settings

let () = Js.Unsafe.global##.draw_grammar_on_canvas_ := Js.wrap_callback draw_grammar_on_canvas
