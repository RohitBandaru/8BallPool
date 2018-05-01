module Html = Dom_html
let jstr = Js.string
let document = Html.window##.document

let pi = acos (-1.)

let bg_color = jstr "#BDC3C7"
let ball_color = jstr "red"


let draw_background canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in
    ctx##rect 0. 0. cw ch;
    ctx##.fillStyle := bg_color;
    ctx##fill

let draw_ball canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let bx = 128. in
  let by = 128. in
  let brad = 16. in
    ctx##beginPath;
    ctx##rect bx by brad brad;
    (* ctx##arc bx by brad 0. pi true; *)
    ctx##.strokeStyle := ball_color;
    ctx##stroke

let start _ =
  let canvas =
    Js.Opt.get
      (Js.Opt.bind ( Html.document##getElementById (jstr "canvas"))
        Html.CoerceTo.canvas)
      (fun () ->
        Printf.printf "Cant find canvas \n";
        assert false ) in
  draw_background canvas;
  draw_ball canvas


let _ = Html.window##.onload := Html.handler (fun _ -> start (); Js._true)