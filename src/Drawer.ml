open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

module DefaultPrimarySettings = struct
  let rule_label_font_size = 15.
  let space_between_rules = 10.
  let definitions_font_size = 12.
  let line_width = 2.
end

module DefaultSecondarySettings = struct
  let arrow_size = 2.
  let dead_end_size = 4.
  let minimal_horizontal_spacing = 1.
  let minimal_vertical_spacing = 1.
  let start_radius = Fl.sqrt 2.
  let stop_radius = 1.5 *. Fl.sqrt 2.
  let turn_radius = 4.
end

module Make(C: JsOfOCairo.S)(PrimarySettings: sig
  val rule_label_font_size: float
  val space_between_rules: float
  val definitions_font_size: float
  val line_width: float
end)(SecondarySettings: sig
  val arrow_size: float
  val dead_end_size: float
  val minimal_horizontal_spacing: float
  val minimal_vertical_spacing: float
  val start_radius: float
  val stop_radius: float
  val turn_radius: float
end) = struct
  module S = struct
    include PrimarySettings

    let arrow_size = line_width *. SecondarySettings.arrow_size
    let dead_end_size = line_width *. SecondarySettings.dead_end_size
    let minimal_horizontal_spacing = line_width *. SecondarySettings.minimal_horizontal_spacing
    let minimal_vertical_spacing = line_width *. SecondarySettings.minimal_vertical_spacing
    let start_radius = line_width *. SecondarySettings.start_radius
    let stop_radius = line_width *. SecondarySettings.stop_radius
    let turn_radius = line_width *. SecondarySettings.turn_radius

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

  (* @todo Display text in fixed-width font *)
  module Terminal = TextSymbol(struct
    type t = Grammar.Terminal.t
    let text = Grammar.Terminal.value
  end)(Bricks.RoundedRectangleText)

  module NonTerminal = TextSymbol(struct
    type t = Grammar.NonTerminal.t
    let text = Grammar.NonTerminal.name
  end)(Bricks.RectangleText)

  module Special = TextSymbol(struct
    type t = Grammar.Special.t
    let text = Grammar.Special.value
  end)(Bricks.PointyRectangleText)

  module rec Sequence: sig
    val measure: Grammar.Sequence.t -> context:C.context -> float * float * float
    val draw: Grammar.Sequence.t -> context:C.context -> unit
  end = struct
    let measure sequence ~context =
      let elements = Grammar.Sequence.elements sequence in
      let (advance, ascent, descent) =
        elements
        |> Li.map ~f:(Definition.measure ~context)
        |> measure_sequence
      in
      (advance +. (S.minimal_horizontal_spacing *. Fl.of_int (Li.size elements - 1)), ascent, descent)

    let draw sequence ~context =
      sequence
      |> Grammar.Sequence.elements
      |> Li.iter_i ~f:(fun i definition ->
        if i <> 0 then Bricks.Advance.draw S.minimal_horizontal_spacing ~context;
        Definition.draw definition ~context
      )
  end

  and Repetition: sig
    val measure: Grammar.Repetition.t -> context:C.context -> float * float * float
    val draw: Grammar.Repetition.t -> context:C.context -> unit
  end = struct
    let measure repetition =
      make_measure (fun context ->
        let (forward_advance, forward_ascent, forward_descent) =
          repetition
          |> Grammar.Repetition.forward
          |> Definition.measure ~context
        and (backward_advance, backward_ascent, backward_descent) =
          repetition
          |> Grammar.Repetition.backward
          |> Definition.measure ~context
        in
        let advance = (2. *. S.turn_radius +. S.line_width +. Fl.max forward_advance backward_advance)
        and ascent = Fl.max S.half_line_width forward_ascent
        and descent = backward_descent +. Fl.max
          (2. *. S.turn_radius)
          (forward_descent +. S.minimal_vertical_spacing +. backward_ascent)
        in
        (advance, ascent, descent)
      )

    let draw repetition =
      make_draw (fun context ->
        let forward = Grammar.Repetition.forward repetition
        and backward = Grammar.Repetition.backward repetition in
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
    let measure exception_ =
      make_measure (fun context ->
        let base = Grammar.Except.base exception_
        and except = Grammar.Except.except exception_ in
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

    let draw exception_ =
      make_draw (fun context ->
        let base = Grammar.Except.base exception_
        and except = Grammar.Except.except exception_ in
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
    let measure alternative =
      make_measure (fun context ->
        let elements = Grammar.Alternative.elements alternative in
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

    let draw alternative =
      make_draw (fun context ->
        let elements = Grammar.Alternative.elements alternative in
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
    let measure_label rule =
      make_measure (fun context ->
        let name = Grammar.Rule.name rule in
        C.set_font_size context S.rule_label_font_size;
        Bricks.Text.measure (sprintf "%s:" name) ~context
      )

    let draw_label rule =
      make_draw (fun context ->
        let name = Grammar.Rule.name rule in
        let (_, label_ascent, label_descent) = measure_label rule ~context in
        C.set_font_size context S.rule_label_font_size;
        C.translate context ~x:0. ~y:label_ascent;
        Bricks.Text.draw (sprintf "%s:" name) ~context;
        (0., label_ascent +. label_descent)
      )

    let measure_definition rule =
      make_measure (fun context ->
        let definition = Grammar.Rule.definition rule in
        let (definition_advance, definition_ascent, definition_descent) = Definition.measure definition ~context
        and (start_advance, start_ascent, start_descent) = Bricks.Start.measure ~context
        and (stop_advance, stop_ascent, stop_descent) = Bricks.Stop.measure ~context in
        let advance = start_advance +. definition_advance +. stop_advance
        and ascent = Fl.max (Fl.max start_ascent definition_ascent) stop_ascent
        and descent = Fl.max (Fl.max start_descent definition_descent) stop_descent in
        (advance, ascent, descent)
      )

    let draw_definition rule =
      make_draw (fun context ->
        C.set_line_width context S.line_width;
        C.set_line_cap context C.ROUND;
        let definition = Grammar.Rule.definition rule in
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

  let measure grammar =
    make_measure (fun context ->
      let rules = Grammar.rules grammar in
      rules
      |> Li.map ~f:(Rule.measure ~context)
      |> Li.fold ~init:(0., -.S.space_between_rules) ~f:(fun (width, height) (w, h) ->
        (Fl.max width w, height +. S.space_between_rules +. h)
      )
    )

  let draw grammar =
    make_draw (fun context ->
      let rules = Grammar.rules grammar in
      let (_, height) = measure grammar ~context in
      rules
      |> Li.iter ~f:(fun rule ->
        Rule.draw rule ~context;
        C.translate context ~x:0. ~y:S.space_between_rules
      );
      (0., height)
    )
end
