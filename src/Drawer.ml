open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

(* @todo Make all lengths, font sizes, line widths, etc. parameters.
This will help validate the drawing algorithm (because it will need to be more robust) *)
module Make(C: JsOfOCairo.S) = struct
  module Bricks = struct
    module Text = struct
      let measure t ~font_size ~context =
        C.save context;
        C.set_font_size context font_size;
        let {C.ascent; descent; _} = C.font_extents context in
        let height = ascent +. descent in
        let {C.x_advance=width; _} = C.text_extents context t in
        C.restore context;
        (width, height)

      let draw t ~x ~font_size ~context =
        C.save context;
        C.set_font_size context font_size;
        C.translate context ~x ~y:0.;
        let {C.ascent; descent; _} = C.font_extents context in
        C.move_to context ~x:0. ~y:((ascent -. descent) /. 2.);
        C.show_text context t;
        C.restore context
    end

    module Start = struct
      let base = 10.

      let measure ~context:_ =
        base

      let draw ~context =
        C.move_to context ~x:0. ~y:0.;
        C.arc context ~x:(base /. 3.) ~y:0. ~r:(base /. 3.) ~a1:0. ~a2:(2. *. Math.pi);
        C.fill context;
        C.move_to context ~x:0. ~y:0.;
        C.line_to context ~x:base ~y:0.;
        C.stroke context;
        C.translate context ~x:base ~y:0.
    end

    module Stop = struct
      let base = 10.

      let measure ~context =
        base +. (C.get_line_width context) /. 2.

      let draw ~context =
        C.arc context ~x:(base /. 2.) ~y:0. ~r:(base /. 3.) ~a1:0. ~a2:(2. *. Math.pi);
        C.fill context;
        C.arc context ~x:(base /. 2.) ~y:0. ~r:(base /. 2.) ~a1:0. ~a2:(2. *. Math.pi);
        C.stroke context;
        C.translate context ~x:base ~y:0.
    end

    module Arrow = struct
      let length = 10.

      let measure ~context:_ =
        length

      let draw ~context =
        C.move_to context ~x:0. ~y:0.;
        C.rel_line_to context ~x:(length -. 5.) ~y:0.;
        C.stroke context;
        C.move_to context ~x:(length -. 5.5) ~y:(-4.);
        C.line_to context ~x:(length -. 0.5) ~y:0.;
        C.line_to context ~x:(length -. 5.5) ~y:4.;
        C.fill context;
        C.translate context ~x:length ~y:0.
    end

    module Segment = struct
      let length = 10.

      let measure ~context:_ =
        length

      let draw ~context =
        C.move_to context ~x:0. ~y:0.;
        C.rel_line_to context ~x:length ~y:0.;
        C.stroke context;
        C.translate context ~x:length ~y:0.
    end

    module RoundedRectangleText = struct
      let base = 10.

      let measure value ~context =
        let (r, _) = Text.measure value ~font_size:10. ~context
        and h = base +. (C.get_line_width context) /. 2. in
        (r +. 2. *. base, h, h)

      let draw value ~context =
        let (r, _, _) = measure value ~context in
        C.arc context ~x:base ~y:0. ~r:base ~a1:(Math.pi /. 2.) ~a2:(-.Math.pi /. 2.);
        C.line_to context ~x:(r -. base) ~y:(-.base);
        C.arc context ~x:(r -. base) ~y:0. ~r:base ~a1:(-.Math.pi /. 2.) ~a2:(Math.pi /. 2.);
        C.Path.close context;
        C.stroke context;
        Text.draw value ~x:base ~font_size:10. ~context;
        C.translate context ~x:r ~y:0.
    end

    module RectangleText = struct
      let base = 10.

      let measure value ~context =
        let (r, _) = Text.measure value ~font_size:10. ~context
        and h = base +. (C.get_line_width context) /. 2. in
        (r +. 2. *. base, h, h)

      let draw value ~context =
        let (r, _, _) = measure value ~context in
        C.rectangle context ~x:0. ~y:(-.base) ~w:r ~h:(2. *. base);
        C.stroke context;
        Text.draw value ~x:base ~font_size:10. ~context;
        C.translate context ~x:r ~y:0.
    end
  end

  module Terminal = struct
    let measure {Grammar.Terminal.value} ~context =
      let arrow_width = Bricks.Arrow.measure ~context
      and (value_width, value_up, value_down) = Bricks.RoundedRectangleText.measure value ~context
      and segment_width = Bricks.Segment.measure ~context in
      (arrow_width +. value_width +. segment_width, value_up, value_down)

    let draw {Grammar.Terminal.value} ~context =
      Bricks.Arrow.draw ~context;
      Bricks.RoundedRectangleText.draw value ~context;
      Bricks.Segment.draw ~context
  end

  module NonTerminal = struct
    let measure {Grammar.NonTerminal.name} ~context =
      let arrow_width = Bricks.Arrow.measure ~context
      and (name_width, name_up, name_down) = Bricks.RectangleText.measure name ~context
      and segment_width = Bricks.Segment.measure ~context in
      (arrow_width +. name_width +. segment_width, name_up, name_down)

    let draw {Grammar.NonTerminal.name} ~context =
      Bricks.Arrow.draw ~context;
      Bricks.RectangleText.draw name ~context;
      Bricks.Segment.draw ~context
  end

  module rec Sequence: sig
    val measure: Grammar.Sequence.t -> context:C.context -> float * float * float
    val draw: Grammar.Sequence.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Sequence.elements} ~context =
      elements
      |> Li.map ~f:(Definition.measure ~context)
      |> Li.fold ~init:(0., 0., 0.) ~f:(fun (r, u, d) (r', u', d') ->
        (r +. r', Fl.max u u', Fl.max d d')
      )

    let draw {Grammar.Sequence.elements} ~context =
      elements
      |> Li.iter ~f:(Definition.draw ~context)
  end

  and Definition: sig
    val measure: Grammar.Definition.t -> context:C.context -> float * float * float
    val draw: Grammar.Definition.t -> context:C.context -> unit
  end = struct
    let measure definition ~context =
      match definition with
        | Grammar.Definition.Terminal x -> Terminal.measure x ~context
        | Grammar.Definition.NonTerminal x -> NonTerminal.measure x ~context
        | Grammar.Definition.Sequence x -> Sequence.measure x ~context

    let draw definition ~context =
      match definition with
        | Grammar.Definition.Terminal x -> Terminal.draw x ~context
        | Grammar.Definition.NonTerminal x -> NonTerminal.draw x ~context
        | Grammar.Definition.Sequence x -> Sequence.draw x ~context
  end

  module Rule = struct
    let font_size = 13.

    let measure {Grammar.Rule.name; definition} ~context =
      C.save context;
      C.set_font_size context font_size;
      let {C.ascent; descent; _} = C.font_extents context in
      let label_height = ascent +. descent in
      let {C.x_advance=label_width; _} = C.text_extents context (sprintf "%s:" name) in
      C.restore context;
      let (definition_width, definition_up, definition_down) = Definition.measure definition ~context in
      let rule_width = Bricks.Start.measure ~context +. definition_width +. Bricks.Stop.measure ~context in
      let rule_height = definition_up +. definition_down in
      (Fl.max label_width rule_width, label_height +. 5. +. rule_height)

    let draw {Grammar.Rule.name; definition} ~context =
      C.save context;

      C.save context;
      C.set_font_size context font_size;
      let {C.ascent; descent; _} = C.font_extents context in
      C.move_to context ~x:0. ~y:ascent;
      C.show_text context (sprintf "%s:" name);
      C.restore context;

      C.translate context ~x:0. ~y:(ascent +. descent +. 5.);

      let (_, up, _) = Definition.measure definition ~context in
      C.translate context ~x:0. ~y:up;
      Bricks.Start.draw ~context;
      Definition.draw definition ~context;
      Bricks.Stop.draw ~context;

      C.restore context;
  end

  let measure {Grammar.rules} ~context =
    (* @todo (in General) PairList.unzip, FloatList.sum, FloatList.max *)
    rules
    |> Li.map ~f:(Rule.measure ~context)
    |> Li.fold ~init:(0., -10.) ~f:(fun (w, h) (w', h') ->
      (Fl.max w w', h +. 10. +. h')
    )

  let draw {Grammar.rules} ~context =
    (* @todo assert our assumptions about the transform matrix:
      - scale is same for x and y
      - angle is 0. *)
    C.save context;
    C.set_line_width context 2.;
    rules
    |> Li.iter ~f:(fun rule ->
      let (_, h) = Rule.measure rule ~context in
      Rule.draw rule ~context;
      C.translate context ~x:0. ~y:(h +. 10.)
    );
    C.restore context
end
