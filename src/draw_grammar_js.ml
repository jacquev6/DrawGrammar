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

let bool_setting default = object%js (_)
  val control_type_ = Js.string "select"
  val setting_type_ = Js.string "bool"
  val default_value_ = Js.bool default
end

let float_setting default = object%js (_)
  val control_type_ = Js.string "select"
  val setting_type_ = Js.string "float"
  val default_value_ = default
end

class type transformation_settings = object
  method simplify: bool Js.t Js.prop
  method inline: Js.js_string Js.t Js.js_array Js.t Js.prop
end

let transformation_settings_description = object%js (_)
  val simplify = bool_setting true
  val inline = object%js (_)
    val control_type_ = Js.string "rules_list"
  end
end

class type primary_settings = object
  method rule_label_font_size_: float Js.prop
  method space_between_rules_: float Js.prop
  method definitions_font_size_: float Js.prop
  method line_width_: float Js.prop
end

let primary_settings_description = object%js (_)
  val rule_label_font_size_ = float_setting Drawer.DefaultPrimarySettings.rule_label_font_size
  val space_between_rules_ = float_setting Drawer.DefaultPrimarySettings.space_between_rules
  val definitions_font_size_ = float_setting Drawer.DefaultPrimarySettings.definitions_font_size
  val line_width_ = float_setting Drawer.DefaultPrimarySettings.line_width
end

class type secondary_settings = object
  method arrow_size_: float Js.prop
  method dead_end_size_: float Js.prop
  method minimal_vertical_spacing_: float Js.prop
  method minimal_horizontal_spacing_: float Js.prop
  method start_radius_: float Js.prop
  method stop_radius_: float Js.prop
  method turn_radius_: float Js.prop
  method ellipsis_size_: float Js.prop
end

let secondary_settings_description = object%js (_)
  val arrow_size_ = float_setting Drawer.DefaultSecondarySettings.arrow_size
  val dead_end_size_ = float_setting Drawer.DefaultSecondarySettings.dead_end_size
  val minimal_vertical_spacing_ = float_setting Drawer.DefaultSecondarySettings.minimal_vertical_spacing
  val minimal_horizontal_spacing_ = float_setting Drawer.DefaultSecondarySettings.minimal_horizontal_spacing
  val start_radius_ = float_setting Drawer.DefaultSecondarySettings.start_radius
  val stop_radius_ = float_setting Drawer.DefaultSecondarySettings.stop_radius
  val turn_radius_ = float_setting Drawer.DefaultSecondarySettings.turn_radius
  val ellipsis_size_ = float_setting Drawer.DefaultSecondarySettings.ellipsis_size
end

let draw grammar (canvas: Dom_html.element Js.t) (transformation_settings: transformation_settings Js.t) (primary_settings: primary_settings Js.t) (secondary_settings: secondary_settings Js.t) =
  let grammar =
    let inline =
      transformation_settings##.inline
      |> Js.to_array
      |> Li.of_array
      |> Li.map ~f:Js.to_string
    in
    inline
    |> Li.fold ~init:grammar ~f:Grammar.inline
    |> Grammar.rules
    |> Li.filter ~f:(fun rule ->
      not (StrLi.contains inline (Grammar.Rule.name rule))
    )
    |> Grammar.grammar
  in
  let grammar =
    if Js.to_bool transformation_settings##.simplify then
      Grammar.simplify grammar
    else
      grammar
  in
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> Exn.failure "Not a canvas") in
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
    let ellipsis_size = secondary_settings##.ellipsis_size_
  end) in
  let (w, h) = Drawer.measure grammar ~context in
  canvas##.width := 1 + Int.of_float w;
  canvas##.height := 1 + Int.of_float h;
  Drawer.draw grammar ~context

let draw_grammar =
  object%js (_)
    val transformation_settings_description_ = transformation_settings_description

    val primary_settings_description_ = primary_settings_description

    val secondary_settings_description_ = secondary_settings_description

    val syntaxes =
      Parse.Syntax.all
      |> Li.map ~f:(fun syntax ->
        object%js (_)
          val value = Js.string (Parse.Syntax.to_string syntax)

          val description = Js.string (Parse.Syntax.description syntax)

          val online_reference_ = Js.string (Parse.Syntax.online_reference syntax)
        end
      )
      |> Li.to_array

    method draw_on_canvas_ syntax grammar canvas settings =
      let grammar = parse_grammar syntax grammar in
      draw grammar canvas settings

    method list_rules_ syntax grammar =
      parse_grammar syntax grammar
      |> Grammar.rules
      |> Li.map ~f:(fun rule ->
        rule
        |> Grammar.Rule.name
        |> Js.string
      )
      |> Li.to_array
      |> Js.array
  end

let () = Js.export "DrawGrammar" draw_grammar
