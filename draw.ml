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

let draw_ball canvas ball =
  let ctx = canvas##getContext (Html._2d_) in
  let t_o = table_off +. rail_off in
  let (bx', by') = get_position ball in
  let (bx, by) = (bx'+.t_o, by'+.t_o) in
  let brad = 11.4 in
  let ballsrc = jstr (get_color ball) in
  let ballimg = Html.createImg document in
  ballimg##.src := ballsrc;
  ctx##drawImage_withSize ballimg (bx-.brad) (by-.brad) (2.*.brad) (2.*.brad)
    (* ctx##beginPath;
    ctx##arc bx by brad 0. (2.0*.pi) Js._true;
    ctx##.fillStyle := ball_color;
    ctx##fill *)

let draw_stick canvas =

  let cue_pos = get_position (List.find (fun b -> get_id b = 0 ) (get_balls !cur_state)) in
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

let draw_gameover canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in
  let (imgw, imgh) = (540., 200.) in
  let winner_id = get_winner_id !cur_state in
  let gameoversrc = jstr ("img/txt-player" ^ (string_of_int winner_id) ^ "win.png") in
  let gameoverimg = Html.createImg document in
  ctx##.fillStyle := jstr("black");
  ctx##fillRect 0. 0. cw ch;
  gameoverimg##.src := gameoversrc;
  ctx##drawImage gameoverimg (cw/.2. -. imgw/.2.) (ch/.2. -. imgh/.2.)

let draw_cue_scratch canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let t_o = table_off +. rail_off in
  let ball = search_ball !cur_state 0 in
  let (bx, by) = (!mouseX, !mouseY) in
  let brad = 11.4 in
  let ballsrc = jstr (get_color ball) in
  let ballimg = Html.createImg document in
  ballimg##.src := ballsrc;
  ctx##drawImage_withSize ballimg (bx-.brad) (by-.brad) (2.*.brad) (2.*.brad)

let draw_state canvas =

  let ball_lst = get_balls !cur_state in
  Firebug.console##log ("ahh" ^ string_of_int (List.length (ball_lst)));
  match !cur_mode with
  | SCRATCH -> begin
      Firebug.console##log ("IN SCRATCH STATE");

      List.iter (fun b -> draw_ball canvas b )
      (List.filter (fun b -> get_id b <> 0) ball_lst);
      draw_cue_scratch canvas
    end
  | PTURN ->
    Firebug.console##log ("IN PTURN STATE");
    draw_stick canvas;
      List.iter (fun b -> draw_ball canvas b ) ball_lst
  | GAMEOVER -> draw_gameover canvas;
  | SIMULATE | _ -> List.iter (fun b -> draw_ball canvas b ) ball_lst


let draw_b2sink canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let t_o = table_off +. rail_off in
  let logic = get_logic !cur_state in
  let (p1, p2) =
    (if logic.player.id = 0 then
      (logic.player, logic.other_player)
    else
      (logic.other_player, logic.player)) in
  let (p1b2s, p2b2s) =  ((fst (List.split p1.balls_left)), (fst (List.split p2.balls_left))) in
  let brad = 11.4 in
  let offset = brad *. 2.5 in
  let (bx, by) = (table_off +. brad, 845.) in
  List.iteri
    (fun i b_id -> begin
         try
           let ball = search_ball !cur_state b_id in
           let (bx', by') = (bx +. (offset *. (float_of_int i)), by) in
           let ballsrc = jstr (get_color ball) in
           let ballimg = Html.createImg document in
           ballimg##.src := ballsrc;
           ctx##drawImage_withSize ballimg (bx'-.brad) (by'-.brad) (2.*.brad) (2.*.brad)
         with
         | _ -> ()
    end ) p1b2s;
  let (bx, by) = (table_off +. 1175. +. brad, 845.) in
  List.iteri
    (fun i b_id -> begin
         try
      let ball = search_ball !cur_state b_id in
      let (bx', by') = (bx -. (offset *. (float_of_int i)), by) in
      let ballsrc = jstr (get_color ball) in
      let ballimg = Html.createImg document in
      ballimg##.src := ballsrc;
      ctx##drawImage_withSize ballimg (bx'-.brad) (by'-.brad) (2.*.brad) (2.*.brad)
        with | _ -> ()
    end ) p2b2s

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
  ctx##drawImage_withSize txt8ballimg   0. 0. 250. 75.;
  ctx##drawImage_withSize txtp1turnimg  table_off 740. 210. 60.;
  ctx##drawImage_withSize txtp2turnimg (table_off+.960.) 740. 210. 60.;
  ctx##drawImage_withSize txtb2sinkimg  table_off 780. 240. 60.;
  ctx##drawImage_withSize txtb2sinkimg (table_off+.960.) 780. 240. 60.;
  if !cur_mode = SCRATCH then
    ctx##drawImage_withSize txtscratchimg (table_off +.500.) 740. 180. 60.;
  draw_b2sink canvas

let draw_pocket canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let t_o = table_off +. rail_off in
  List.iter (fun pocket -> begin
    let (px', py') = pocket in
    let (px, py) = (px' +. t_o, py' +. t_o) in
    let pocketsrc = jstr "img/0.png" in
    let pocketimg = Html.createImg document in
    pocketimg##.src := pocketsrc;
    ctx##drawImage_withSize pocketimg (px-.32.) (py-.32.) 64. 64.
    end ) pockets

let draw canvas =
  draw_background canvas;
  draw_hud canvas;
  draw_state canvas;
  (* draw_pocket canvas; *)
  ()

let move canvas =
  let speed = (!power/.100.)*.(1000.) in
  Firebug.console##log (Printf.sprintf "speed %f" speed);
  let ball_lst = ball_locations (!cur_state) in
  cur_state := update_cue_ball_velocity !cur_state ((speed *. (cos (!stick_angle+.pi))),
    (speed *. (sin (!stick_angle+.pi))));
  (*cur_state := (get_logic !cur_state, List.map
    (fun b -> if get_id b = 0 then change_velocity b move else b ) ball_lst);*)
  power := 0.0;
  cur_mode := SIMULATE;
  ()

let keydown canvas event =
  let () = match event##.keyCode with
    | 13 -> (* enter *)if (!power > 10.) then
        move canvas
    | 37 -> stick_angle := !stick_angle -. 0.1;
      draw_stick canvas(* left *)
    | 38 -> (* up *) if (!power < 100.) then power := !power +. 10.0;
      draw_stick canvas
    | 39 -> stick_angle := !stick_angle +. 0.1;
      draw_stick canvas(* left *)
    | 40 -> (* down *)if (!power > 0.) then power := !power -. 1.0;
      draw_stick canvas
    | 81 -> (* q key *) cur_mode := GAMEOVER
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
  let ball_rad = 11.4 in
  let x = fst position in
  let y = snd position in
  let fake_ball = create_ball "Cue" 0  Cue "img/0.png" (0.,0.) position 0.156 ball_rad; (*Cue*) in
  not ((List.exists (fun x -> if (get_id x <> 0) then is_overlap x fake_ball else false) (get_balls !cur_state)) ||
       (x < ball_rad || x > 1024. -. ball_rad || y < ball_rad || y > 512. -. ball_rad))

let handle_invalid_scratch canvas =
  debug "ope invalid scratch";
  let ctx = canvas##getContext (Html._2d_) in
  let txtinvalidsrc = jstr "img/txt-invalid.png" in
  let txtinvalidimg = Html.createImg document in
  txtinvalidimg##.src := txtinvalidsrc;
  ctx##drawImage_withSize txtinvalidimg (table_off +. 460.) 750. 120. 25.


let mouseup canvas event =
  match !cur_mode with
  | SCRATCH -> begin
    let rect = canvas##getBoundingClientRect in
    let canvasX = (float_of_int event##.clientX) -. rect##.left in
    let canvasY = (float_of_int event##.clientY) -. rect##.top in
    let updateX = canvasX -. (table_off+.rail_off) in
    let updateY = canvasY -. (table_off+.rail_off) in
    let _ =
      if valid_pos (updateX, updateY) then
        begin
          cur_state := update_cue_ball_position !cur_state (updateX, updateY);
          cur_mode  := PTURN
        end
      else
        handle_invalid_scratch canvas
    in Js._true
  end
  | _ -> Js._false

let event_list = ref []

let rec loop canvas =
  draw canvas;
  let logic = get_logic !cur_state in
  let balls = get_balls !cur_state in
  let tdelta = 0.001 in
  let _ = match !cur_mode with
  | SIMULATE -> let ball_lst = ball_locations (!cur_state) in
    let (physics, events) = simulate_timestep ball_lst tdelta in
    if is_converged ball_lst
    then begin cur_state := (EightBall.next_state logic (!event_list), physics);
        event_list := [];
        cur_mode := PTURN
      end
    else
      cur_state := (logic, physics);
      event_list := List.append events !event_list;
  | SCRATCH -> ()
  | PTURN -> ()
  | GAMEOVER -> ()
  in
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
