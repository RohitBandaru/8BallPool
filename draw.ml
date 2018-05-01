module Html = Dom_html
let jstr = Js.string
let document = Html.window##.document

let pi = acos (-1.)

let bg_color = jstr "#BDC3C7"
let brdr_color = jstr "#af7418"
let pool_color = jstr "#0a6c03"
let ball_color = jstr "red"


let draw_background canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in
    ctx##rect 0. 0. cw ch;
    ctx##.fillStyle := pool_color;
    ctx##fill

let draw_board canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in
  let border_rad = 20. in
    ctx##.fillStyle := jstr("brown");
    ctx##fillRect 0. 0. border_rad ch;
    ctx##fillRect 0. 0. cw border_rad;
    ctx##fillRect 0. (ch-.border_rad) cw border_rad;
    ctx##fillRect (cw-.border_rad) 0. border_rad ch

let draw_ball canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let bx = 128. in
  let by = 128. in
  let brad = 16. in
    ctx##beginPath;
    ctx##arc bx by brad 0. (2.*.pi) Js._true;
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
  draw_board canvas;
  draw_ball canvas


let _ = Html.window##.onload := Html.handler (fun _ -> start (); Js._true)