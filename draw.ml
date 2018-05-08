module Html = Dom_html
let jstr = Js.string
let document = Html.window##.document

let pi = acos (-1.)

let bg_color = jstr "#BDC3C7"
let ball_color = jstr "red"
let stick_color = jstr "blue"

let stick_angle = ref 0.0
let power = ref 0.0

let draw_background canvas =
  let ctx = canvas##getContext (Html._2d_) in
  let cw = float canvas##.width in
  let ch = float canvas##.height in
    ctx##rect 0. 0. cw ch;
    ctx##.fillStyle := bg_color;
    ctx##fill

let draw_ball canvas off =
  let ctx = canvas##getContext (Html._2d_) in
  let bx = (128. +. off) in
  let by = 128. in
  let brad = 16. in
    ctx##beginPath;
    ctx##arc bx by brad 0. (2.0*.pi) Js._true;
    ctx##.fillStyle := ball_color;
    ctx##fill

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

let draw canvas off = 
  draw_background canvas;
  draw_ball canvas off;
  draw_stick canvas; 
  ()

let move canvas = 
  ()

let keydown canvas event =
  let () = match event##.keyCode with
    |40 -> (* enter *)if (!power > 10.)
      move canvas
    |37 -> stick_angle := !stick_angle -. 1.0;
      draw_stick canvas(* left *)
    |38 -> (* up *) if (!power < 100.) then power := !power +. 1.0;
      draw_stick canvas
    |39 -> stick_angle := !stick_angle +. 1.0;
      draw_stick canvas(* left *)
    |40 -> (* down *)if (!power > 0.) then power := !power -. 1.0;
      draw_stick canvas
    | _ -> () (* other *)
  in Js._true


let rec start _ =
  let canvas =
    Js.Opt.get
      (Js.Opt.bind ( Html.document##getElementById (jstr "canvas"))
        Html.CoerceTo.canvas)
      (fun () ->
        Printf.printf "Cant find canvas \n";
        assert false ) in
  Html.document##.onkeydown :=
      (Html.handler
         (fun e -> keydown canvas e));
  let rec looper off = 
    draw canvas off;
    Html.window##requestAnimationFrame(
      Js.wrap_callback (fun (t:float) -> looper (off+.(t/.100.0)))
    ) |> ignore
  in looper 0.0


let _ = 
  Html.window##.onload := Html.handler (fun _ -> start (); Js._true);


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
  