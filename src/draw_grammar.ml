open General.Abbr

let draw_grammar_on_canvas syntax grammar canvas =
  let canvas = Js.Opt.get (Dom_html.CoerceTo.canvas canvas) (fun _ -> failwith "Not a canvas") in
  let context = canvas##getContext (Dom_html._2d_) in
  let width = (context##measureText (grammar))##width in
  canvas##width <- Int.of_float (20. +. width);
  canvas##height <- 200
  begin
    context##moveTo (0., 0.);
    context##lineTo (100., 100.);
    context##stroke ();
    context##fillText (syntax, 10., 10.);
    context##fillText (grammar, 20., 20.);
  end;

let () = Js.Unsafe.global##draw_grammar_on_canvas_ (* why do I need a last undesrcore? *) <- Js.wrap_callback draw_grammar_on_canvas
