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

let cur_state = init_state EightBallSolo

let scratch = ref (get_logic cur_state).scratch

let mouseX = ref 0.

let mouseY = ref 0.

let table_off = 76.

let test_ball =
  match init_pos with
  | [] -> failwith "Empty"
  | h::t -> h

let (tbx, tby) = get_position test_ball

let ball_color = jstr "red"
let stick_color = jstr "blue"

let stick_angle = ref 0.0
let power = ref 0.0

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
  let (bx', by') = get_position ball in
  let (bx, by) =
  if !scratch && get_id ball = 0 then (!mouseX, !mouseY) else (bx'+.t_o, by'+.t_o) in
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

let draw_stick canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let bx = (128.) in
  let by = 128. in
    ctx##beginPath;
    ctx##moveTo 0.0 0.0;
    ctx##lineTo (bx+.300.+.(!stick_angle)) (by+.150.);
    ctx##.lineWidth := 10.;
    ctx##.fillStyle := stick_color;
    ctx##stroke

let draw canvas =
  draw_background canvas;
  draw_state canvas;
  draw_stick canvas;
  ()

let move canvas =
  ()

let keydown canvas event =
  let () = match event##.keyCode with
    | 13 -> (* enter *)if (!power > 10.) then
      move canvas;
    | 37 -> stick_angle := !stick_angle -. 1.0;
      draw_stick canvas(* left *)
    | 38 -> (* up *) if (!power < 100.) then power := !power +. 1.0;
      draw_stick canvas
    | 39 -> stick_angle := !stick_angle +. 1.0;
      draw_stick canvas(* left *)
    | 40 -> (* down *)if (!power > 0.) then power := !power -. 1.0;
      draw_stick canvas
    | 82 -> (* s key *) scratch := not !scratch
    | _ -> Firebug.console##log (Printf.sprintf "scratch %b" !scratch)
  in Js._true

let mousemove canvas event =
  let rect = canvas##getBoundingClientRect in
  let canvasX = (float_of_int event##.clientX) -. rect##.left in
  let canvasY = (float_of_int event##.clientY) -. rect##.top in
  mouseX := canvasX;
  mouseY := canvasY;
  (* let _ = Firebug.console##log (Printf.sprintf "canvasx %0.4f canvasy %0.4f" canvasX canvasY)
  in *) Js._true

let valid_pos position =
  (* TODO validate if the ball is within bounds of the table, and not in any pockets fall range *)
  true

let update_cue_pos position =
  (* TODO udpate the state with new ball position *)
  ()

let handle_invalid_scratch _ =
  (* TODO display some sort of error message indicating scratch pos was out of bounds *)
  ()

let mouseup canvas event =
  let rect = canvas##getBoundingClientRect in
  let canvasX = (float_of_int event##.clientX) -. rect##.left in
  let canvasY = (float_of_int event##.clientY) -. rect##.top in

  let _ =
    if valid_pos (canvasX, canvasY) then
      begin
        update_cue_pos ();
        scratch := false
      end
    else
      handle_invalid_scratch ()
  in Js._true

let rec loop canvas =
  draw canvas;
  Html.window##requestAnimationFrame(
    Js.wrap_callback (fun (t:float) -> loop canvas)) |> ignore

let rec init _ =
  let canvas =
    Js.Opt.get
      (Js.Opt.bind ( Html.document##getElementById (jstr "canvas"))
      Html.CoerceTo.canvas)
      (fun () -> Printf.printf "Cant find canvas \n"; assert false ) in
  document##.onkeydown :=
    (Html.handler (fun e -> keydown canvas e));

  document##.onmousemove :=
    (Html.handler (fun e -> mousemove canvas e));

  document##.onmouseup :=
    (Html.handler (fun e -> mouseup canvas e));

  loop canvas

let _ =
  Html.window##.onload := Html.handler (fun _ -> init (); Js._true);


  (*

  let _ = Html.addEventListener
      document Html.Event.keydown (Html.handler keydown)
      Js._true in
let rec start2 _ =
  ()



  let _ = Html.addEventListener
      document Html.Event.keydown (Html.handler keyup)
      Js._true in

  let main =
    Js.Opt.get (document##getElementById (jstr "main"))
      (fun () -> assert false)
  in

  Dom.appendChild main
    (button "reset" main);


  let _ = Html.addEventListener
      document Html.Event.keydown (Html.handler keyup)
  in

let button name main =
  let res = document##createDocumentFragment in
  let input = Html.createInput ~_type:(jstr "submit") document in
  input##.value := jstr name;
  input##.onclick := Html.handler (fun _ ->
          let div = Html.createDiv document in
          Dom.appendChild main div;
          Js._false);
  Dom.appendChild res input;
  res


let keyup event =
  let () = match event##keyCode with
    | 83 -> ()
    | _ -> ()
  in Js._true



  *)
