open Data
open Ball

module Html = Dom_html

let jstr = Js.string
let document = Html.window##.document

let pi = acos (-1.)

let bg_color = jstr "#BDC3C7"
let brdr_color = jstr "#af7418"
let pool_color = jstr "#0a6c03"
let ball_color = jstr "white"

let stripe_color = jstr "#22A7F0"
let solid_color = jstr "#C3272B"

let init_pos = eight_ball_init_ball_pos

let table_off = 76.

let test_ball =
  match init_pos with
  | [] -> failwith "Empty"
  | h::t -> h

let (tbx, tby) = get_position test_ball

let draw_background canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let tablesrc = jstr "img/table-scaled.png" in
  let tableimg = Html.createImg document in
  tableimg##.src := tablesrc;
  ctx##drawImage tableimg 0. 0.

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

let draw_ball canvas ball off =
  let ctx = canvas##getContext (Html._2d_) in
  let t_o = table_off in
    let (bx, by) = get_position ball in
    let (bx, by) = (bx+.t_o, by+.t_o) in
    let brad = 11.4 in
    let ballsrc = jstr (get_color ball) in
    let ballimg = Html.createImg document in
    ballimg##.src := ballsrc;
    ctx##drawImage_withSize ballimg (bx-.brad) (by-.brad) (2.*.brad) (2.*.brad)
    (* ctx##beginPath;
    ctx##arc bx by brad 0. (2.0*.pi) Js._true;
    ctx##.fillStyle := ball_color;
    ctx##fill *)

let draw_state canvas =
  List.iter (fun b -> draw_ball canvas b 0. ) init_pos

let draw canvas off =
  draw_background canvas;
  draw_state canvas;
  ()

let rec start _ =
(*   let main =
    Js.Opt.get (document##getElementById (jstr "main"))
      (fun () -> assert false)
  in *)
  let canvas =
    Js.Opt.get
      (Js.Opt.bind ( Html.document##getElementById (jstr "canvas"))
        Html.CoerceTo.canvas)
      (fun () ->
        Printf.printf "Cant find canvas \n";
        assert false ) in

  let rec looper off =
    draw canvas off;
    Html.window##requestAnimationFrame(
      Js.wrap_callback (fun (t:float) -> looper (off+.1.0))
    ) |> ignore
  in looper 0.0

let _ =
  Html.window##.onload := Html.handler (fun _ -> start (); Js._true);
