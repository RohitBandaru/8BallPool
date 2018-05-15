open Data
open Ball
open Physics

module Html = Dom_html

let jstr = Js.string
let document = Html.window##.document

type mode =
  | PTURN
  | SCRATCH
  | SIMULATE
  | GAMEOVER

let pi = acos (-1.)

let bg_color = jstr "#BDC3C7"

let brdr_color = jstr "#af7418"
let pool_color = jstr "#0a6c03"
let ball_color = jstr "white"

let stripe_color = jstr "#22A7F0"
let solid_color = jstr "#C3272B"

let init_pos = eight_ball_init_ball_pos

let cur_state = ref (init_state EightBall)

let cur_mode = ref PTURN

let mouseX = ref 0.

let mouseY = ref 0.

let rail_off = 76.

let table_off = 75.

let test_ball =
  match init_pos with
  | [] -> failwith "Empty"
  | h::t -> h

let (tbx, tby) = get_position test_ball

let ball_color = jstr "red"
let stick_color = jstr "blue"

let stick_angle = ref 0.0
let power = ref 0.0

let debug str =
  Firebug.console##log str

let draw_background canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in

  let tablesrc = jstr "img/table-scaled.png" in
  let tableimg = Html.createImg document in
  ctx##.fillStyle := jstr("LightSteelBlue ");
  ctx##fillRect 0. 0. cw ch;
  tableimg##.src := tablesrc;
  ctx##drawImage tableimg table_off table_off

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
  let t_o = table_off +. rail_off in
  let (bx', by') = get_position ball in
  let (bx, by) =
  if !cur_mode = SCRATCH && get_id ball = 0 then (!mouseX, !mouseY) else (bx'+.t_o, by'+.t_o) in
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
  let ball_lst = get_balls !cur_state in
  List.iter (fun b -> draw_ball canvas b 0. ) ball_lst

let draw_stick canvas =
  let cue_pos = get_position (List.find (fun b -> get_id b = 0 ) init_pos) in
  let stick_length = 300. in
  let brad = 11.4 in
  let ctx = canvas##getContext (Html._2d_) in
  let rail_off = table_off +. rail_off in
    ctx##beginPath;
    ctx##moveTo (fst cue_pos +. rail_off +.  (brad+. !power)*.(cos !stick_angle))
      (snd cue_pos +. rail_off +.  (brad +. !power)*.(sin !stick_angle));
    ctx##lineTo (fst cue_pos +. rail_off +. (brad +. stick_length +. !power)*.(cos !stick_angle))
      (snd cue_pos +. rail_off +. (brad +. stick_length +. !power)*.(sin !stick_angle));
    ctx##.lineWidth := 7.;
    ctx##.fillStyle := stick_color;
    ctx##stroke

let draw_hud canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let txt8ballsrc   = jstr "img/txt-8ball.png" in
  let txtp1turnsrc  = jstr "img/txt-player1turn.png" in
  let txtp2turnsrc  = jstr "img/txt-player2turn.png" in
  let txtscratchsrc = jstr "img/txt-scratch.png" in
  let txtb2sinksrc  = jstr "img/txt-ballstosink.png" in
  let txt8ballimg   = Html.createImg document in
  let txtp1turnimg  = Html.createImg document in
  let txtp2turnimg  = Html.createImg document in
  let txtscratchimg = Html.createImg document in
  let txtb2sinkimg  = Html.createImg document in
  txt8ballimg##.src   := txt8ballsrc;
  txtp1turnimg##.src  := txtp1turnsrc;
  txtp2turnimg##.src  := txtp2turnsrc;
  txtscratchimg##.src := txtscratchsrc;
  txtb2sinkimg##.src  := txtb2sinksrc;
  ctx##drawImage_withSize txt8ballimg 0. 0. 250. 75.;
  ctx##drawImage_withSize txtp1turnimg table_off 740. 210. 60.;
  ctx##drawImage_withSize txtp2turnimg (table_off+.1000.) 740. 210. 60.;
  ctx##drawImage_withSize txtscratchimg (table_off +.500.) 740. 180. 60.;
  ctx##drawImage_withSize txtb2sinkimg table_off 780. 240. 60.;
  ctx##drawImage_withSize txtb2sinkimg (table_off+.1000.) 780. 240. 60.


let draw canvas =
  draw_background canvas;
  draw_state canvas;
  draw_stick canvas;
  draw_hud canvas;
  ()

let move canvas =
  ()

let keydown canvas event =
  let () = match event##.keyCode with
    | 13 -> (* enter *)if (!power > 10.) then
      move canvas;
    | 37 -> stick_angle := !stick_angle -. 0.1;
      draw_stick canvas(* left *)
    | 38 -> (* up *) if (!power < 100.) then power := !power +. 1.0;
      draw_stick canvas
    | 39 -> stick_angle := !stick_angle +. 0.1;
      draw_stick canvas(* left *)
    | 40 -> (* down *)if (!power > 0.) then power := !power -. 1.0;
      draw_stick canvas
    | 82 -> (* r key *) cur_mode := SCRATCH
    | 83 -> (* s key *)
      begin
        let logic = get_logic !cur_state in
        let balls = get_balls !cur_state in
        let tdelta = 0.001 in
        cur_state := (logic, fst (simulate_timestep balls tdelta))
      end
    | _ ->
      begin
        let balls = get_balls !cur_state in
        List.iter
        (fun b -> debug (Printf.sprintf
          "[id: %d, xcor %f, ycor %f]\n" (get_id b) (fst (get_position b)) (snd (get_position b))) ) balls;
        debug "/////////////////////////////////////////////////////////////////"
      end
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

let handle_invalid_scratch _ =
  (* TODO display some sort of error message indicating scratch pos was out of bounds *)
  ()

let mouseup canvas event =
  let rect = canvas##getBoundingClientRect in
  let canvasX = (float_of_int event##.clientX) -. rect##.left in
  let canvasY = (float_of_int event##.clientY) -. rect##.top in
  let updateX = canvasX -. (table_off+.rail_off) in
  let updateY = canvasY -. (table_off+.rail_off) in
  let _ =
    if valid_pos (canvasX, canvasY) then
      begin
        cur_state := update_cue_ball_position !cur_state (updateX, updateY);
        cur_mode := PTURN
      end
    else
      handle_invalid_scratch ()
  in Js._true

let rec loop canvas =
  draw canvas;
  let logic = get_logic !cur_state in
  let balls = get_balls !cur_state in
  let tdelta = 0.001 in
  (* cur_state := (logic, fst simulate_timestep balls); *)
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
