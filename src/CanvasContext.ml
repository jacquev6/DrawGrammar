open General.Abbr

type t = Dom_html.canvasRenderingContext2D Js.t

(* Text *)
module TextExtents = struct
  type t = {
    advance: float;
    (* ascend: float; *)
    (* descend: float; *)
  }
end

let measure_text context ~t =
  let advance = (context##measureText (Js.string t))##width in
  {TextExtents.advance}

let show_text context ~x ~y ~t =
  context##fillText (Js.string t, x, y)

(* Lines *)
let move_to context ~x ~y =
  context##moveTo (x, y)

let line_to context ~x ~y =
  context##lineTo (x, y)

let stroke context =
  context##stroke ()
