open General.Abbr

let parse_grammar syntax grammar =
  let syntax =
    syntax
    |> Js.to_string
    |> Parse.Syntax.of_string
  in
  try
    grammar
    |> Js.to_string
    |> Parse.parse_string ~syntax
  with
    | Parse.Errors.Lexing message ->
      Js.raise_js_error (object%js (_)
        val mutable name = Js.string "lexing error"
        val mutable message = Js.string message
        val mutable stack = Js.undefined
        method toString = Js.string message
      end)
    | Parse.Errors.Parsing message ->
      Js.raise_js_error (object%js (_)
        val mutable name = Js.string "parsing error"
        val mutable message = Js.string message
        val mutable stack = Js.undefined
        method toString = Js.string message
      end)

class type primary_settings = object
  method simplify: bool Js.t Js.prop
  method rule_label_font_size_: float Js.prop
  method space_between_rules_: float Js.prop
  method definitions_font_size_: float Js.prop
  method line_width_: float Js.prop
end

let default_primary_settings = object%js (_)
  val simplify = Js.bool true
  val rule_label_font_size_ = Drawer.DefaultPrimarySettings.rule_label_font_size
  val space_between_rules_ = Drawer.DefaultPrimarySettings.space_between_rules
  val definitions_font_size_ = Drawer.DefaultPrimarySettings.definitions_font_size
  val line_width_ = Drawer.DefaultPrimarySettings.line_width
end

class type secondary_settings = object
  method arrow_size_: float Js.prop
  method dead_end_size_: float Js.prop
  method minimal_vertical_spacing_: float Js.prop
  method minimal_horizontal_spacing_: float Js.prop
  method start_radius_: float Js.prop
  method stop_radius_: float Js.prop
  method turn_radius_: float Js.prop
end

let default_secondary_settings = object%js (_)
  val arrow_size_ = Drawer.DefaultSecondarySettings.arrow_size
  val dead_end_size_ = Drawer.DefaultSecondarySettings.dead_end_size
  val minimal_vertical_spacing_ = Drawer.DefaultSecondarySettings.minimal_vertical_spacing
  val minimal_horizontal_spacing_ = Drawer.DefaultSecondarySettings.minimal_horizontal_spacing
  val start_radius_ = Drawer.DefaultSecondarySettings.start_radius
  val stop_radius_ = Drawer.DefaultSecondarySettings.stop_radius
  val turn_radius_ = Drawer.DefaultSecondarySettings.turn_radius
end

let draw grammar (canvas: Dom_html.element Js.t) (primary_settings: primary_settings Js.t) (secondary_settings: secondary_settings Js.t) =
  let grammar =
    if Js.to_bool primary_settings##.simplify then
      Grammar.simplify grammar
    else
      grammar
  in
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  let context = JsOfOCairo.create canvas in
  let module Drawer = Drawer.Make(JsOfOCairo)(struct
    let rule_label_font_size = primary_settings##.rule_label_font_size_
    let space_between_rules = primary_settings##.space_between_rules_
    let definitions_font_size = primary_settings##.definitions_font_size_
    let line_width = primary_settings##.line_width_
  end)(struct
    let arrow_size = secondary_settings##.arrow_size_
    let dead_end_size = secondary_settings##.dead_end_size_
    let minimal_vertical_spacing = secondary_settings##.minimal_vertical_spacing_
    let minimal_horizontal_spacing = secondary_settings##.minimal_horizontal_spacing_
    let start_radius = secondary_settings##.start_radius_
    let stop_radius = secondary_settings##.stop_radius_
    let turn_radius = secondary_settings##.turn_radius_
  end) in
  let (w, h) = Drawer.measure grammar ~context in
  canvas##.width := 1 + Int.of_float w;
  canvas##.height := 1 + Int.of_float h;
  Drawer.draw grammar ~context

let draw_grammar =
  object%js (_)
    val default_primary_settings_ = default_primary_settings

    val default_secondary_settings_ = default_secondary_settings

    method on_canvas_ syntax grammar canvas settings =
      let grammar = parse_grammar syntax grammar in
      draw grammar canvas settings
  end

let () = Js.export "DrawGrammar" draw_grammar
