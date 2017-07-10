open General.Abbr

let sprintf = OCamlStandard.Printf.sprintf

(* @todo Make all lengths, font sizes, line widths, etc. parameters.
This will help validate the drawing algorithm (because it will need to be more robust) *)
module Make(C: JsOfOCairo.S) = struct
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
      let measure t ~font_size ~context =
        C.save context;
        C.set_font_size context font_size;
        let {C.ascent; descent; _} = C.font_extents context in
        let height = ascent +. descent in
        let {C.x_advance=width; _} = C.text_extents context t in
        C.restore context;
        (width, height)

      let draw t ~x ~font_size ~context =
        let (w, _) = measure t ~font_size ~context in
        C.save context;
        if is_forward context then begin
          C.translate context ~x ~y:0.;
        end else begin
          C.rotate context ~angle:Math.pi;
          C.translate context ~x:(-.w -. x) ~y:0.;
        end;
        C.set_font_size context font_size;
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

    module DeadEnd = struct
      let base = 10.

      let measure ~context =
        base +. (C.get_line_width context) /. 2.

      let draw ~context =
        C.move_to context ~x:0. ~y:0.;
        C.line_to context ~x:(base /. 2.) ~y:0.;
        C.move_to context ~x:0. ~y:(base /. 2.);
        C.line_to context ~x:base ~y:(-.base /. 2.);
        C.move_to context ~x:0. ~y:(-.base /. 2.);
        C.line_to context ~x:base ~y:(base /. 2.);
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

    module Advance = struct
      let draw length ~context =
        C.move_to context ~x:0. ~y:0.;
        C.rel_line_to context ~x:length ~y:0.;
        C.stroke context;
        C.translate context ~x:length ~y:0.
    end

    module Segment = struct
      let length = 10.

      let measure ~context:_ =
        length

      let draw ~context =
        Advance.draw length ~context
    end

    module Turns = struct
      let base = 5.

      let left ~context =
        C.translate context ~x:base ~y:0.;
        C.arc_negative context ~x:(-.base) ~y:(-.base) ~r:base ~a1:(Math.pi /. 2.) ~a2:0.;
        C.rotate context ~angle:(-.Math.pi /. 2.);
        C.stroke context;
        C.translate context ~x:base ~y:0.

      let right ~context =
        C.translate context ~x:base ~y:0.;
        C.arc context ~x:(-.base) ~y:base ~r:base ~a1:(3. *. Math.pi /. 2.) ~a2:0.;
        C.rotate context ~angle:(Math.pi /. 2.);
        C.stroke context;
        C.translate context ~x:base ~y:0.

      let get ~context =
        if is_forward context then
          (left, right)
        else
          (right, left)
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

    module PointyRectangleText = struct
      let base = 10.

      let measure value ~context =
        let (r, _) = Text.measure value ~font_size:10. ~context
        and h = base +. (C.get_line_width context) /. 2. in
        (r +. 2. *. base, h, h)

      let draw value ~context =
        let (r, _, _) = measure value ~context in
        C.move_to context ~x:base ~y:base;
        C.line_to context ~x:0. ~y:0.;
        C.line_to context ~x:base ~y:(-.base);
        C.line_to context ~x:(r -. base) ~y:(-.base);
        C.line_to context ~x:r ~y:0.;
        C.line_to context ~x:(r -. base) ~y:base;
        C.Path.close context;
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

  module Special = struct
    let measure {Grammar.Special.value} ~context =
      let arrow_width = Bricks.Arrow.measure ~context
      and (value_width, value_up, value_down) = Bricks.PointyRectangleText.measure value ~context
      and segment_width = Bricks.Segment.measure ~context in
      (arrow_width +. value_width +. segment_width, value_up, value_down)

    let draw {Grammar.Special.value} ~context =
      Bricks.Arrow.draw ~context;
      Bricks.PointyRectangleText.draw value ~context;
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

  and Alternative: sig
    val measure: Grammar.Alternative.t -> context:C.context -> float * float * float
    val draw: Grammar.Alternative.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Alternative.elements} ~context =
      let measures =
        elements
        |> Li.map ~f:(Definition.measure ~context)
      in
      let (r, h) =
        measures
        |> Li.fold ~init:(0., -5.) ~f:(fun (r, h) (r', u', d') ->
          (Fl.max r r', h +. 5. +. u' +. d')
        )
      and (_, u, _) = Li.head measures in
      (r +. 20., u, h -. u)

    let draw ({Grammar.Alternative.elements} as alternative) ~context =
      C.save context;

      let (width, _, _) = measure alternative ~context
      and (turn_left, turn_right) = Bricks.Turns.get ~context
      and element = Li.head elements
      and elements = Li.tail elements in
      let (w, _, d) = Definition.measure element ~context in
      C.save context;
      Bricks.Segment.draw ~context;
      Bricks.Advance.draw ((width -. 20. -. w) /. 2.) ~context;
      Definition.draw element ~context;
      Bricks.Advance.draw ((width -. 20. -. w) /. 2.) ~context;
      Bricks.Segment.draw ~context;
      C.restore context;
      turn_right ~context;
      elements
      |> Li.fold_i ~init:(d -. 10.) ~f:(fun i prev_d element ->
        let (w, u, d) = Definition.measure element ~context in
        Bricks.Advance.draw (prev_d +. 5. +. u) ~context;
        C.save context;
        turn_left ~context;
        Bricks.Advance.draw ((width -. 20. -. w) /. 2.) ~context;
        Definition.draw element ~context;
        Bricks.Advance.draw ((width -. 20. -. w) /. 2.) ~context;
        turn_left ~context;
        Bricks.Advance.draw (prev_d +. 5. +. u) ~context;
        if i = 0 then turn_right ~context;
        C.restore context;
        d
      )
      |> ignore;
      C.restore context;

      C.translate context ~x:width ~y:0.
  end

  and Repetition: sig
    val measure: Grammar.Repetition.t -> context:C.context -> float * float * float
    val draw: Grammar.Repetition.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Repetition.forward; backward} ~context =
      let (fr, fu, fd) = Definition.measure forward ~context
      and (br, bu, bd) = Definition.measure backward ~context in
      (20. +. Fl.max fr br, fu, fd +. 5. +. bu +. bd)

    let draw {Grammar.Repetition.forward; backward} ~context =
      let (_, turn_right) = Bricks.Turns.get ~context
      and (fr, _, fd) = Definition.measure forward ~context
      and (br, bu, _) = Definition.measure backward ~context in
      C.save context;

      Bricks.Segment.draw ~context;

      Bricks.Advance.draw ((Fl.max fr br -. fr) /. 2.) ~context;
      Definition.draw forward ~context;
      Bricks.Advance.draw ((Fl.max fr br -. fr) /. 2.) ~context;

      turn_right ~context;
      Bricks.Advance.draw (fd +. bu -. 5.) ~context;
      turn_right ~context;

      Bricks.Advance.draw ((Fl.max fr br -. br) /. 2.) ~context;
      Definition.draw backward ~context;
      Bricks.Advance.draw ((Fl.max fr br -. br) /. 2.) ~context;

      turn_right ~context;
      Bricks.Advance.draw (fd +. bu -. 5.) ~context;
      turn_right ~context;

      C.restore context;

      C.translate context ~x:(10. +. Fl.max fr br) ~y:0.;

      Bricks.Segment.draw ~context
  end

  and Except: sig
    val measure: Grammar.Except.t -> context:C.context -> float * float * float
    val draw: Grammar.Except.t -> context:C.context -> unit
  end = struct
    let measure {Grammar.Except.base; except} ~context =
      let (base_r, base_u, base_d) = Definition.measure base ~context
      and (except_r, except_u, except_d) = Definition.measure except ~context in
      (20. +. Fl.max base_r (except_r +. Bricks.DeadEnd.measure ~context), base_u, base_d +. 5. +. except_u +. except_d)

    let draw {Grammar.Except.base; except} ~context =
      let (turn_left, turn_right) = Bricks.Turns.get ~context
      and (base_advance, base_up, _) = Definition.measure base ~context
      and (except_advance, _, except_down) = Definition.measure except ~context in
      let w = Fl.max base_advance (except_advance +. Bricks.DeadEnd.measure ~context) in

      C.save context;
      Bricks.Segment.draw ~context;
      Definition.draw except ~context;
      Bricks.DeadEnd.draw ~context;
      C.restore context;

      turn_right ~context;
      Bricks.Advance.draw (except_down +. base_up -. 5.) ~context;
      turn_left ~context;
      Bricks.Advance.draw ((w -. base_advance) /. 2.) ~context;
      Definition.draw base ~context;
      Bricks.Advance.draw ((w -. base_advance) /. 2.) ~context;
      turn_left ~context;
      Bricks.Advance.draw (except_down +. base_up -. 5.) ~context;
      turn_right ~context;
  end

  and Definition: sig
    val measure: Grammar.Definition.t -> context:C.context -> float * float * float
    val draw: Grammar.Definition.t -> context:C.context -> unit
  end = struct
    let measure definition ~context =
      match definition with
        | Grammar.Definition.Null -> let h = (C.get_line_width context) /. 2. in (0., h, h)
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
    let font_size = 13.

    let measure {Grammar.Rule.name; definition} ~context =
      C.save context;
      C.set_font_size context font_size;
      let {C.ascent; descent; _} = C.font_extents context in
      let label_height = ascent +. descent in
      let {C.x_advance=label_width; _} = C.text_extents context (sprintf "%s:" name) in
      C.restore context;
      let (definition_width, definition_up, definition_down) = Definition.measure definition ~context in
      let end_u_d = 0.5 *. Bricks.Stop.measure ~context in
      let rule_width = Bricks.Start.measure ~context +. definition_width +. Bricks.Stop.measure ~context in
      let rule_height = (Fl.max end_u_d definition_up) +. (Fl.max end_u_d definition_down) in
      (Fl.max label_width rule_width, label_height +. rule_height)

    let draw {Grammar.Rule.name; definition} ~context =
      C.save context;

      C.save context;
      C.set_font_size context font_size;
      let {C.ascent; descent; _} = C.font_extents context in
      C.move_to context ~x:0. ~y:ascent;
      C.show_text context (sprintf "%s:" name);
      C.restore context;

      C.translate context ~x:0. ~y:(ascent +. descent);

      let (_, up, _) = Definition.measure definition ~context in
      let end_u_d = 0.5 *. Bricks.Stop.measure ~context in
      C.translate context ~x:0. ~y:(Fl.max end_u_d up);
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
