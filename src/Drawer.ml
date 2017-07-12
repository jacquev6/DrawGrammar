open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module DefaultSettings = struct
  let arrow_size = None
  let dead_end_size = None
  let definitions_font_size = None
  let line_width = None
  let minimal_vertical_spacing = None
  let minimal_horizontal_spacing = None
  let rule_label_font_size = None
  let space_between_rules = None
  let start_radius = None
  let stop_radius = None
  let turn_radius = None
end

module Make(C: JsOfOCairo.S)(SettingOverrides: sig
  val arrow_size: float option
  val dead_end_size: float option
  val definitions_font_size: float option
  val line_width: float option
  val minimal_vertical_spacing: float option
  val minimal_horizontal_spacing: float option
  val rule_label_font_size: float option
  val space_between_rules: float option
  val start_radius: float option
  val stop_radius: float option
  val turn_radius: float option
end) = struct
  module S = struct
    let arrow_size = Opt.value_def SettingOverrides.arrow_size ~def:5.
    let dead_end_size = Opt.value_def SettingOverrides.dead_end_size ~def:12.
    let definitions_font_size = Opt.value_def SettingOverrides.definitions_font_size ~def:12.
    let line_width = Opt.value_def SettingOverrides.line_width ~def:2.
    let minimal_vertical_spacing = Opt.value_def SettingOverrides.minimal_vertical_spacing ~def:5.
    let minimal_horizontal_spacing = Opt.value_def SettingOverrides.minimal_horizontal_spacing ~def:5.
    let start_radius = Opt.value_def SettingOverrides.start_radius ~def:6.
    let stop_radius = Opt.value_def SettingOverrides.stop_radius ~def:9.
    let turn_radius = Opt.value_def SettingOverrides.turn_radius ~def:5.
    let space_between_rules = Opt.value_def SettingOverrides.space_between_rules ~def:10.
    let rule_label_font_size = Opt.value_def SettingOverrides.rule_label_font_size ~def:15.

    let half_line_width = line_width /. 2.
  end

  let save_restore ~context f =
    C.save context;
    let r =
      try
        f context
      with e -> C.restore context; raise e
    in
    C.restore context;
    r

  let make_measure = save_restore

  let make_draw ~context f =
    let (x, y) = save_restore ~context f in
    C.Path.clear context;
    C.translate context ~x ~y

  let measure_sequence measures =
    measures
    |> Li.fold ~init:(0., 0., 0.) ~f:(fun (advance, ascent, descent) (advance', ascent', descent') ->
      (advance +. advance', Fl.max ascent ascent', Fl.max descent descent')
    )

  module Bricks = struct
    let is_forward context =
      (* @todo Remove this: we would fail drawing on an already rotated or scaled context.
      Pass is_forward as a parameter when recursing in the definitions. *)
      let {C.xx; xy; yx; yy; _} = C.get_matrix context in
      assert (Fl.abs xy < 1e-6);
      assert (Fl.abs yx < 1e-6);
      assert (Fl.abs (xx -. yy) < 1e-6);
      assert (Fl.abs (xx -. 1.) < 1e-6 || Fl.abs (xx +. 1.) < 1e-6);
      Fl.abs (xx -. 1.) < 1e-6

    module Text = struct
      let measure t =
        make_measure (fun context ->
          let {C.ascent; descent; _} = C.font_extents context
          and {C.x_advance; _} = C.text_extents context t in
          let h = (ascent +. descent) /. 2. in
          (x_advance, h, h)
        )

      let draw t =
        make_draw (fun context ->
          let {C.ascent; descent; _} = C.font_extents context in
          if is_forward context then begin
            C.move_to context ~x:0. ~y:((ascent -. descent) /. 2.);
          end else begin
            let (advance, _, _) = measure t ~context in
            C.move_to context ~x:advance ~y:((descent -. ascent) /. 2.);
            C.identity_matrix context;
          end;
          C.show_text context t;
          (0., 0.)
        )
    end

    module Start = struct
      let measure =
        make_measure (fun _ ->
          let half_height = Fl.max S.start_radius S.half_line_width in
          (3. *. S.start_radius, half_height, half_height)
        )

      let draw =
        make_draw (fun context ->
          C.arc context ~x:S.start_radius ~y:0. ~r:S.start_radius ~a1:0. ~a2:(2. *. Math.pi);
          C.fill context;
          C.move_to context ~x:S.start_radius ~y:0.;
          C.line_to context ~x:(3. *. S.start_radius) ~y:0.;
          C.stroke context;
          (3. *. S.start_radius, 0.)
        )
    end

    module Stop = struct
      let measure =
        make_measure (fun _ ->
          (2. *. S.stop_radius, S.stop_radius, S.stop_radius)
        )

      let draw =
        make_draw (fun context ->
          C.move_to context ~x:0. ~y:0.;
          C.line_to context ~x:S.stop_radius ~y:0.;
          C.stroke context;
          C.arc context ~x:S.stop_radius ~y:0. ~r:S.stop_radius ~a1:0. ~a2:(2. *. Math.pi);
          C.fill context;
          C.arc context ~x:S.stop_radius ~y:0. ~r:(0.6 *. S.stop_radius) ~a1:0. ~a2:(2. *. Math.pi);
          save_restore ~context (fun _ ->
            C.set_source_rgb context ~r:1. ~g:1. ~b:1.;
            C.fill context;
          );
          C.arc context ~x:S.stop_radius ~y:0. ~r:(0.3 *. S.stop_radius) ~a1:0. ~a2:(2. *. Math.pi);
          C.fill context;
          (2. *. S.stop_radius, 0.)
        )
    end

    module DeadEnd = struct
      let half_size = S.dead_end_size /. 2.

      let measure =
        make_measure (fun _ ->
          let half_height = Fl.max S.half_line_width half_size in
          (2. *. half_size, half_height, half_height)
        )

      let draw =
        make_draw (fun context ->
          C.move_to context ~x:0. ~y:0.;
          C.line_to context ~x:half_size ~y:0.;
          C.move_to context ~x:0. ~y:half_size;
          C.line_to context ~x:S.dead_end_size ~y:(-.half_size);
          C.move_to context ~x:0. ~y:(-.half_size);
          C.line_to context ~x:S.dead_end_size ~y:half_size;
          C.stroke context;
          (half_size, 0.)
        )
    end

    module Arrow = struct
      let measure =
        make_measure (fun _ ->
          (S.arrow_size, S.arrow_size, S.arrow_size)
        )

      let draw =
        make_draw (fun context ->
          C.move_to context ~x:0. ~y:(-.S.arrow_size);
          C.line_to context ~x:S.arrow_size ~y:0.;
          C.line_to context ~x:0. ~y:S.arrow_size;
          C.fill context;
          (S.arrow_size, 0.)
        )
    end

    module Advance = struct
      let measure length =
        make_measure (fun _ ->
          (length, S.half_line_width, S.half_line_width)
        )

      let draw length =
        make_draw (fun context ->
          C.move_to context ~x:0. ~y:0.;
          C.line_to context ~x:length ~y:0.;
          C.stroke context;
          (length, 0.)
        )
    end

    module Turns = struct
      let left ~context =
        C.translate context ~x:S.turn_radius ~y:0.;
        C.arc_negative context ~x:(-.S.turn_radius) ~y:(-.S.turn_radius) ~r:S.turn_radius ~a1:(Math.pi /. 2.) ~a2:0.;
        C.stroke context;
        C.rotate context ~angle:(-.Math.pi /. 2.);
        C.translate context ~x:S.turn_radius ~y:0.

      let right ~context =
        C.translate context ~x:S.turn_radius ~y:0.;
        C.arc context ~x:(-.S.turn_radius) ~y:S.turn_radius ~r:S.turn_radius ~a1:(3. *. Math.pi /. 2.) ~a2:0.;
        C.stroke context;
        C.rotate context ~angle:(Math.pi /. 2.);
        C.translate context ~x:S.turn_radius ~y:0.

      let get ~context =
        if is_forward context then
          (left, right)
        else
          (right, left)
    end

    module AnyRectangleText = struct
      let measure text =
        make_measure (fun context ->
          C.set_font_size context S.definitions_font_size;
          let (text_advance, text_ascent, text_descent) = Text.measure text ~context in
          let height = 2. *. S.line_width +. 2. *. S.minimal_vertical_spacing +. text_ascent +. text_descent in
          (text_advance +. height, height /. 2., height /. 2.)
        )

      let draw_text text =
        make_draw (fun context ->
          let (advance, _, _) = measure text ~context in
          C.set_font_size context S.definitions_font_size;
          let (text_advance, _, _) = Text.measure text ~context in
          C.translate context ~x:((advance -. text_advance) /. 2.) ~y:0.;
          Text.draw text ~context;
          (0., 0.)
        )
    end

    module RoundedRectangleText = struct
      include AnyRectangleText

      let draw text =
        make_draw (fun context ->
          draw_text text ~context;
          let (advance, ascent, _) = measure text ~context in
          let radius = ascent -. S.half_line_width in
          C.arc context ~x:ascent ~y:0. ~r:radius ~a1:(Math.pi /. 2.) ~a2:(-.Math.pi /. 2.);
          C.arc context ~x:(advance -. ascent) ~y:0. ~r:radius ~a1:(-.Math.pi /. 2.) ~a2:(Math.pi /. 2.);
          C.Path.close context;
          C.move_to context ~x:(advance -. S.half_line_width) ~y:0.;
          C.line_to context ~x:advance ~y:0.;
          C.stroke context;
          (advance, 0.)
        )
    end

    module RectangleText = struct
      include AnyRectangleText

      let draw text =
        make_draw (fun context ->
          draw_text text ~context;
          let (advance, h, _) = measure text ~context in
          let radius = h -. S.half_line_width in
          C.rectangle context ~x:S.half_line_width ~y:(-.radius) ~w:(advance -. S.line_width) ~h:(2. *. radius);
          C.stroke context;
          (advance, 0.)
        )
    end

    module PointyRectangleText = struct
      include AnyRectangleText

      let draw text =
        make_draw (fun context ->
          draw_text text ~context;
          let (advance, h, _) = measure text ~context in
          let radius = h -. S.half_line_width in
          C.move_to context ~x:radius ~y:(-.radius);
          C.line_to context ~x:S.half_line_width ~y:(-.h /. 2.);
          C.line_to context ~x:S.half_line_width ~y:(h /. 2.);
          C.line_to context ~x:radius ~y:radius;
          C.line_to context ~x:(advance -. radius) ~y:radius;
          C.line_to context ~x:(advance -. S.half_line_width) ~y:(h /. 2.);
          C.line_to context ~x:(advance -. S.half_line_width) ~y:(-.h /. 2.);
          C.line_to context ~x:(advance -. radius) ~y:(-.radius);
          C.Path.close context;
          C.stroke context;
          (advance, 0.)
        )
    end
  end

  let draw_centered ~total_advance ~advance ~context f =
    let centering_advance = (total_advance -. advance) /. 2. in
    Bricks.Advance.draw centering_advance ~context;
    f ~context;
    Bricks.Advance.draw centering_advance ~context;

  module TextSymbol(Get: sig
    type t
    val text: t -> string
  end)(Rectangle: sig
    val measure: string -> context:C.context -> float * float * float
    val draw: string -> context:C.context -> unit
  end) = struct
    let measure symbol =
      make_measure (fun context ->
        measure_sequence [
          Bricks.Arrow.measure ~context;
          Rectangle.measure (Get.text symbol) ~context;
          Bricks.Advance.measure S.arrow_size ~context;
        ]
      )

    let draw symbol ~context =
      Bricks.Arrow.draw ~context;
      Rectangle.draw (Get.text symbol) ~context;
      Bricks.Advance.draw S.arrow_size ~context;
  end

  module Terminal = TextSymbol(struct
    type t = Grammar.Terminal.t
    let text {Grammar.Terminal.value} =
      value
  end)(Bricks.RoundedRectangleText)

  module NonTerminal = TextSymbol(struct
    type t = Grammar.NonTerminal.t
    let text {Grammar.NonTerminal.name} =
      name
  end)(Bricks.RectangleText)

  module Special = TextSymbol(struct
    type t = Grammar.Special.t
    let text {Grammar.Special.value} =
      value
  end)(Bricks.PointyRectangleText)

  module rec Sequence: sig
    val measure: Grammar.Sequence.t -> context:C.context -> float * float * float
    val draw: Grammar.Sequence.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Sequence.elements} ~context =
      let (advance, ascent, descent) =
        elements
        |> Li.map ~f:(Definition.measure ~context)
        |> measure_sequence
      in
      (advance +. (S.minimal_horizontal_spacing *. Fl.of_int (Li.size elements - 1)), ascent, descent)

    let draw {Grammar.Sequence.elements} ~context =
      elements
      |> Li.iter_i ~f:(fun i definition ->
        if i <> 0 then Bricks.Advance.draw S.minimal_horizontal_spacing ~context;
        Definition.draw definition ~context
      )
  end

  and Repetition: sig
    val measure: Grammar.Repetition.t -> context:C.context -> float * float * float
    val draw: Grammar.Repetition.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Repetition.forward; backward} =
      make_measure (fun context ->
        let (forward_advance, forward_ascent, forward_descent) = Definition.measure forward ~context
        and (backward_advance, backward_ascent, backward_descent) = Definition.measure backward ~context in
        let advance = (2. *. S.turn_radius +. S.line_width +. Fl.max forward_advance backward_advance)
        and ascent = Fl.max S.half_line_width forward_ascent
        and descent = backward_descent +. Fl.max
          (2. *. S.turn_radius)
          (forward_descent +. S.minimal_vertical_spacing +. backward_ascent)
        in
        (advance, ascent, descent)
      )

    let draw ({Grammar.Repetition.forward; backward} as repetition) =
      make_draw (fun context ->
        let (_, turn_right) = Bricks.Turns.get ~context
        and (advance, _, _) = measure repetition ~context
        and (forward_advance, _, forward_descent) = Definition.measure forward ~context
        and (backward_advance, backward_ascent, _) = Definition.measure backward ~context in
        let total_advance = Fl.max forward_advance backward_advance in

        Bricks.Advance.draw (S.turn_radius +. S.half_line_width) ~context;

        draw_centered ~total_advance ~advance:forward_advance ~context (Definition.draw forward);

        save_restore ~context (fun context ->
          Bricks.Advance.draw (S.turn_radius +. S.half_line_width) ~context;
        );

        let vertical_advance = Fl.max 0. (forward_descent +. backward_ascent +. S.minimal_vertical_spacing -. 2. *. S.turn_radius) in
        turn_right ~context;
        Bricks.Advance.draw vertical_advance ~context;
        turn_right ~context;

        draw_centered ~total_advance ~advance:backward_advance ~context (Definition.draw backward);

        turn_right ~context;
        Bricks.Advance.draw vertical_advance ~context;
        turn_right ~context;
        (advance, 0.)
      )
  end

  and Except: sig
    val measure: Grammar.Except.t -> context:C.context -> float * float * float
    val draw: Grammar.Except.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Except.base; except} =
      make_measure (fun context ->
        let (except_advance, except_ascent, except_descent) = measure_sequence [Definition.measure except ~context; Bricks.DeadEnd.measure ~context]
        and (base_advance, base_ascent, base_descent) = Definition.measure base ~context in
        let advance = (4. *. S.turn_radius +. Fl.max except_advance base_advance)
        and ascent = Fl.max S.half_line_width except_ascent
        and descent = base_descent +. Fl.max
          (2. *. S.turn_radius)
          (except_descent +. S.minimal_vertical_spacing +. base_ascent)
        in
        (advance, ascent, descent)
      )

    let draw ({Grammar.Except.base; except} as exception_) =
      make_draw (fun context ->
        let (turn_left, turn_right) = Bricks.Turns.get ~context
        and (advance, _, _) = measure exception_ ~context
        and (except_advance, _, except_descent) = measure_sequence [Definition.measure except ~context; Bricks.DeadEnd.measure ~context]
        and (base_advance, base_ascent, _) = Definition.measure base ~context in
        let total_advance = Fl.max except_advance base_advance in

        save_restore ~context (fun context ->
          Bricks.Advance.draw (2. *. S.turn_radius) ~context;
          Definition.draw except ~context;
          Bricks.DeadEnd.draw ~context;
        );

        let vertical_advance = Fl.max 0. (except_descent +. base_ascent +. S.minimal_vertical_spacing -. 2. *. S.turn_radius) in
        turn_right ~context;
        Bricks.Advance.draw vertical_advance ~context;
        turn_left ~context;
        draw_centered ~total_advance ~advance:base_advance ~context (Definition.draw base);
        turn_left ~context;
        Bricks.Advance.draw vertical_advance ~context;
        turn_right ~context;
        (advance, 0.)
      )
  end

  and Alternative: sig
    val measure: Grammar.Alternative.t -> context:C.context -> float * float * float
    val draw: Grammar.Alternative.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Alternative.elements} =
      make_measure (fun context ->
        let first_element = Li.head elements
        and other_elements = Li.tail elements in
        let (first_advance, first_ascent, first_descent) = Definition.measure first_element ~context in
        let (elements_advance, descent, last_descent) =
          other_elements
          |> Li.fold ~init:(first_advance, 0., first_descent) ~f:(fun (advance, descent, prev_descent) element ->
            let (advance', ascent', descent') = Definition.measure element ~context in
            let advance = Fl.max advance advance'
            and descent = descent +. Fl.max
              (prev_descent +. S.minimal_vertical_spacing +. ascent')
              (2. *. S.turn_radius)
            in
            (advance, descent, descent')
          )
        in
        let advance = 4. *. S.turn_radius +. elements_advance in
        (advance, first_ascent, descent +. last_descent)
      )

    let draw ({Grammar.Alternative.elements} as alternative) =
      make_draw (fun context ->
        let (turn_left, turn_right) = Bricks.Turns.get ~context
        and first_element = Li.head elements
        and other_elements = Li.tail elements in
        let (advance, _, _) = measure alternative ~context
        and (first_advance, _, first_descent) = Definition.measure first_element ~context in
        let total_advance = advance -. 4. *. S.turn_radius in

        save_restore ~context (fun context ->
          Bricks.Advance.draw (2. *. S.turn_radius) ~context;
          draw_centered ~total_advance ~advance:first_advance ~context (Definition.draw first_element);
          Bricks.Advance.draw (2. *. S.turn_radius) ~context;
        );

        other_elements
        |> Li.fold_i ~init:first_descent ~f:(fun i prev_descent element ->
          let (advance, ascent, descent) = Definition.measure element ~context in

          if i = 0 then begin
            turn_right ~context;
          end else begin
            Bricks.Advance.draw (2. *. S.turn_radius) ~context;
          end;

          let vertical_advance =
            Fl.max 0. (prev_descent +. S.minimal_vertical_spacing +. ascent -. 2. *. S.turn_radius)
          in
          Bricks.Advance.draw vertical_advance ~context;
          save_restore ~context (fun context ->
            turn_left ~context;
            draw_centered ~total_advance ~advance ~context (Definition.draw element);
            turn_left ~context;
            Bricks.Advance.draw vertical_advance ~context;

            if i = 0 then begin
              turn_right ~context;
            end else begin
              Bricks.Advance.draw (2. *. S.turn_radius) ~context;
            end;
          );

          descent
        )
        |> ignore;

        (advance, 0.)
      )
  end

  and Definition: sig
    val measure: Grammar.Definition.t -> context:C.context -> float * float * float
    val draw: Grammar.Definition.t -> context:C.context -> unit
  end = struct
    let measure definition ~context =
      match definition with
        | Grammar.Definition.Null -> let h = S.half_line_width in (0., h, h)
        | Grammar.Definition.Terminal x -> Terminal.measure x ~context
        | Grammar.Definition.NonTerminal x -> NonTerminal.measure x ~context
        | Grammar.Definition.Sequence x -> Sequence.measure x ~context
        | Grammar.Definition.Alternative x -> Alternative.measure x ~context
        | Grammar.Definition.Repetition x -> Repetition.measure x ~context
        | Grammar.Definition.Special x -> Special.measure x ~context
        | Grammar.Definition.Except x -> Except.measure x ~context

    let draw definition ~context =
      match definition with
        | Grammar.Definition.Null -> ()
        | Grammar.Definition.Terminal x -> Terminal.draw x ~context
        | Grammar.Definition.NonTerminal x -> NonTerminal.draw x ~context
        | Grammar.Definition.Sequence x -> Sequence.draw x ~context
        | Grammar.Definition.Alternative x -> Alternative.draw x ~context
        | Grammar.Definition.Repetition x -> Repetition.draw x ~context
        | Grammar.Definition.Special x -> Special.draw x ~context
        | Grammar.Definition.Except x -> Except.draw x ~context
  end

  module Rule = struct
    let measure_label {Grammar.Rule.name; _} =
      make_measure (fun context ->
        C.set_font_size context S.rule_label_font_size;
        Bricks.Text.measure (sprintf "%s:" name) ~context
      )

    let draw_label ({Grammar.Rule.name; _} as rule) =
      make_draw (fun context ->
        let (_, label_ascent, label_descent) = measure_label rule ~context in
        C.set_font_size context S.rule_label_font_size;
        C.translate context ~x:0. ~y:label_ascent;
        Bricks.Text.draw (sprintf "%s:" name) ~context;
        (0., label_ascent +. label_descent)
      )

    let measure_definition {Grammar.Rule.definition; _} =
      make_measure (fun context ->
        let (definition_advance, definition_ascent, definition_descent) = Definition.measure definition ~context
        and (start_advance, start_ascent, start_descent) = Bricks.Start.measure ~context
        and (stop_advance, stop_ascent, stop_descent) = Bricks.Stop.measure ~context in
        let advance = start_advance +. definition_advance +. stop_advance
        and ascent = Fl.max (Fl.max start_ascent definition_ascent) stop_ascent
        and descent = Fl.max (Fl.max start_descent definition_descent) stop_descent in
        (advance, ascent, descent)
      )

    let draw_definition ({Grammar.Rule.definition; _} as rule) =
      make_draw (fun context ->
        let (_, definition_ascent, definition_descent) = measure_definition rule ~context in
        C.translate context ~x:0. ~y:definition_ascent;
        Bricks.Start.draw ~context;
        Definition.draw definition ~context;
        Bricks.Stop.draw ~context;
        (0., definition_descent)
      )

    let measure rule =
      make_measure (fun context ->
        let (label_advance, label_ascent, label_descent) = measure_label rule ~context
        and (definition_advance, definition_ascent, definition_descent) = measure_definition rule ~context in
        let width = Fl.max label_advance definition_advance
        and height = label_ascent +. label_descent +. definition_ascent +. definition_descent in
        (width, height)
      )

    let draw rule =
      make_draw (fun context ->
        let (_, height) = measure rule ~context in
        draw_label rule ~context;
        draw_definition rule ~context;
        (0., height)
      )
  end

  let measure {Grammar.rules} =
    make_measure (fun context ->
      C.set_line_width context S.line_width;

      rules
      |> Li.map ~f:(Rule.measure ~context)
      |> Li.fold ~init:(0., -.S.space_between_rules) ~f:(fun (width, height) (w, h) ->
        (Fl.max width w, height +. S.space_between_rules +. h)
      )
    )

  let draw ({Grammar.rules} as grammar) =
    make_draw (fun context ->
      C.set_line_width context S.line_width;

      let (_, height) = measure grammar ~context in
      rules
      |> Li.iter ~f:(fun rule ->
        Rule.draw rule ~context;
        C.translate context ~x:0. ~y:S.space_between_rules
      );
      (0., height)
    )
end
