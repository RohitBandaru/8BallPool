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

let draw_ball canvas off =
  let ctx = canvas##getContext (Html._2d_) in
  let bx = (128. +. off) in
  let by = 128. in
  let brad = 16. in
    ctx##beginPath;
    (* ctx##rect bx by brad brad; *)
    ctx##arc bx by brad 0. (2.0*.pi) Js._true;
    ctx##.fillStyle := ball_color;
    ctx##fill

let draw canvas off = 
  draw_background canvas;
  draw_ball canvas off;
  ()
  

let rec start _ =
  let main =
    Js.Opt.get (document##getElementById (jstr "main"))
      (fun () -> assert false)
  in
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


  (*

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
  