open General.Abbr

let draw_syntax_on_canvas syntax grammar canvas =
    let get_by_id id coerce = Js.Opt.get (coerce (Dom_html.getElementById id)) (fun () -> raise Not_found) in
    let canvas = get_by_id canvas Dom_html.CoerceTo.canvas in
    let context = canvas##getContext (Dom_html._2d_) in
    begin
      context##moveTo (0., 0.);
      context##lineTo (100., 100.);
      context##stroke ();
      context##fillText (syntax, 25., 25.);
      context##fillText (grammar, 25., 50.);
    end

let () = Js.Unsafe.global##draw_syntax_on_canvas_ (* why do I need a last undesrcore? *) <- Js.wrap_callback draw_syntax_on_canvas
