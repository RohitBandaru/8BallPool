open Ball
open Printf
open Data
open Random

type ball = Ball.t

let is_overlap t1 t2 =
  let (x1, y1) = Ball.get_position t1 in
  let (x2, y2) = Ball.get_position t2 in
  let r1 = Ball.get_radius t1 in
  let r2 = Ball.get_radius t2 in
  (x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0 < (r1 +. r2) ** 2.0 -. 0.005 (*fudge factor *)

let dot v1 v2 =
  match v1, v2 with
  | ((v1x, v1y), (v2x, v2y)) -> v1x *. v2x +. v1y *. v2y

(* diff (a1, a2), (b1, b2) is (a1-b1, a2-b2) *)
let diff (a1,a2) (b1,b2) = (a1-.b1, a2-.b2)

let sum (a1,a2) (b1, b2) = (a1+.b1, a2+.b2)

let norm v1 = (dot v1 v1) ** 0.5

let apply_friction ts b=
  let (old_vx, old_vy) = Ball.get_velocity b in

  let (new_v) = (old_vx -. old_vx *. ts *. 0.15, old_vy -. old_vy *. ts *. 0.15) in


  Ball.change_velocity b new_v

let collide t1 t2 =
  (* Algorithm comes fom http://www.petercollingridge.co.uk/pygame-physics-simulation/collisions *)
  let (x1, y1) = Ball.get_position t1 in
  let (x2, y2) = Ball.get_position t2 in
  let dy, dx = (y1 -. y2, x1 -. x2) in
  (*let tangent_angle = atan2 dy dx in
  let norm_angle = 3.1415926 /. 2.0 -. tangent_angle in
  *)
  let norm_angle = atan2 dy dx in

  (* We only care about the velocity perpendicular to the tangent *)
  let norm_vector = (dx, dy) in
  let norm_magnitude = norm norm_vector in
  let norm_vector = (dx /. norm_magnitude  +. (0.025 -. Random.float 0.05)
                    , dy /. norm_magnitude  +. 0.025 -.(Random.float 0.05)) in
  let tan_vector = (-.dy /. norm_magnitude  +. (0.025 -. Random.float 0.05)
                   , dx /. norm_magnitude  +. (0.025 -.Random.float 0.05)) in
  let tangent_angle = atan2 (dx)(-. dy) in

  (* Velocity of ball 1, normal coordinates, before *)
  let v1nb = dot (Ball.get_velocity t1) (norm_vector) in
  (* Velocity of ball 2, normal coordinates, before *)
  let v2nb = dot (Ball.get_velocity t2) (norm_vector) in

  (* Velocities of ball 1 and 2, tangent coordinates *)
  let v1t = dot (Ball.get_velocity t1) tan_vector in
  let v2t = dot (Ball.get_velocity t2) tan_vector in

  let (m1, m2) = (Ball.get_mass t1, Ball.get_mass t2) in
  (* Velocity of ball 1, normal coordinates, after *)
  let v1na = (v1nb *. (m1 -. m2) +. 2.0 *. m2 *. v2nb) /. (m1 +. m2)
             *. (1.0 +. (0.025 -.Random.float 0.05))
  in
  (* Velocity of ball 2, normal coordinates, after *)
  let v2na = ((v2nb *. (m2 -. m1) +. 2.0 *. m1 *. v1nb) /. (m1 +. m2))
             *. (1.0 +. (0.025 -.Random.float 0.05))
  in
  (* Velocity of ball 1, cartesian coordinates, after *)
  let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
              v1t *. sin tangent_angle +. v1na *. sin norm_angle)

  in
  (* Velocity of ball 2, cartesian coordinates, after *)
  let v2ca = (v2t *. cos tangent_angle +. v2na *. cos norm_angle,
              v2t *. sin tangent_angle +. v2na *. sin norm_angle)
  in

  ((Ball.get_id t1, (diff v1ca (Ball.get_velocity t1) )),
    (Ball.get_id t2, (diff v2ca (Ball.get_velocity t2) )))
  (*Ball.change_velocity t1 v1ca, Ball.change_velocity t2 v2ca*)
  (*let new_b1 = Ball.update_position (Ball.change_velocity t1 v1ca) 0.0001 in
  let new_b2 = Ball.update_position (Ball.change_velocity t2 v2ca) 0.0001 in
    (new_b1, new_b2)*)
  



module IntMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let update key new_val map=
  if IntMap.mem key map
  then IntMap.add key (sum (IntMap.find key map) new_val) map
  else IntMap.add key new_val map

let table_top = 0.
let table_bot = 512.
let table_left = 0.
let table_right = 1024.


(* dx, dy for bouncing off each wall *)
let left_dx = 1.
let right_dx = -1.
let left_dy = 0.
let right_dy = 0.
let top_dx = 0.
let bot_dx = 0.
let top_dy = -1.
let bot_dy = 1.

(* dx, dy for boucning off the pool pocket walls *)
let ne_dx = 0.5**0.5
let ne_dy = -.(0.5 ** 0.5)
let nw_dx = 0.5**0.5
let nw_dy = (0.5 ** 0.5)

(*let ne_dx = 1.
  let ne_dy = 0.*)


let rim_width = 22.
let pocket_radius = 32.
let center_pocket_radius = 28.
(* coordinate of the bottom part of the top holes *)
let top_rim_offset = 19.
let bottom_rim_offset = 512. -. top_rim_offset

let pocket_top_y = table_bot +. top_rim_offset -. pocket_radius
let pocket_bot_y = table_top -. top_rim_offset +. pocket_radius
let pocket_left_x = table_left -. rim_width
let pocket_right_x = table_right +. rim_width
let pocket_middle_x = 506.



let pocket_nw = (pocket_left_x, pocket_top_y)
let pocket_sw = (pocket_left_x, pocket_bot_y)
let pocket_n = (pocket_middle_x, pocket_top_y)
let pocket_s = (pocket_middle_x, pocket_bot_y)
let pocket_ne = (pocket_right_x, pocket_top_y)
let pocket_se = (pocket_right_x, pocket_bot_y)


let is_bounce ball =
  let (bx, by) = Ball.get_position ball in
  let r = Ball.get_radius ball in
  let norm_r = r /. (0.8) in
  if
    (* special angled bouncing *)
    (
    (* NE pocket, bottom wall, right -> up *)
      bx >= table_right -. Ball.get_radius ball
      && (by >= top_rim_offset -. r&& by <= top_rim_offset +. rim_width) &&
      ((by-. (table_top +. top_rim_offset +. rim_width)) +.( bx -. table_right +. 1.5 *. rim_width ) >= norm_r)

      ||
      (* SW pocket, top awll, left -> down *)
      bx <= table_left +. Ball.get_radius ball &&

      ((by <= bottom_rim_offset && by >= bottom_rim_offset -. rim_width) &&
       (by-. ( bottom_rim_offset)) +.( bx -. table_left +. rim_width) <= norm_r)


      ||
      (* SW pocket, bottom wall, down -> left *)
      by >= table_bot -. (Ball.get_radius ball) &&
      ((bx <= rim_width +. 2. && bx >= 0.) &&
       (by -. (table_bot +. 0.0 *. rim_width) +. (bx -. table_left) >= norm_r)
      )

      ||
      (* NE pocket, top wall, up -> right *)
      by <= table_top +. (Ball.get_radius ball) &&
      ((bx >= (pocket_right_x -. pocket_radius -. rim_width) && bx <= pocket_right_x -. pocket_radius) &&
       (bx -. (pocket_right_x -. pocket_radius -. rim_width) +. (by -.table_top) <= norm_r)
      )
      ||
      (* NW pocket, top wall, up -> left *)
      by <= table_top +. r &&
      (bx >= 0. && by <= rim_width +. 2.) &&
      ((by +. rim_width) -. (by) <= 0. )
      ||
      (* NW pocket, bottom wall, left -> up *)
      bx <= table_left +. r &&
      (by >= top_rim_offset -. r&& by <= top_rim_offset +. rim_width) &&
      ((by-. (table_top +. top_rim_offset +. rim_width)) +.( bx -. table_left +. 1.5 *. rim_width ) >= norm_r)

      (* SE pocket, top wall, right -> down *)
      ||
      bx >= table_right -. r &&

      ((by <= bottom_rim_offset && by >= bottom_rim_offset -. rim_width) &&
       (by-. ( bottom_rim_offset)) +.( bx -. table_right +. rim_width) >= norm_r)

      ||
      (* SE pocket, bottom wall, down -> right *)
      by >= table_bot -. r &&
      ((bx >= (pocket_right_x -. pocket_radius -. rim_width) && bx <= pocket_right_x -. pocket_radius) &&
       (bx -. (pocket_right_x -. pocket_radius -. rim_width) +. (by -.table_bot) >= norm_r)
      )
      (* N pocket, left wall, up -> right *)
      ||
      (by <= table_top +. r ) &&
      (
        (bx <= pocket_middle_x +.center_pocket_radius +. rim_width
         && bx >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
        (
          bx -. (pocket_middle_x -.center_pocket_radius) +. (by -. table_top) <= norm_r
        )
      )

      (* S pocket, left wall, down -> right *)
      ||
      ( by >= table_bot -. r) &&
      (
        (bx <= pocket_middle_x +. pocket_radius +. rim_width
         && bx >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
        (
          bx -. (pocket_middle_x -. center_pocket_radius) -. (by -. table_bot) <= norm_r
        )
      )

      ||
      (* N pocket, right wall, up -> left *)
      (by <= table_top +. r) &&
      (
        (bx <= pocket_middle_x +. pocket_radius +. rim_width
         && bx >= pocket_middle_x -.center_pocket_radius-. rim_width) &&
        (
          bx -. (pocket_middle_x +. center_pocket_radius ) -. (by -. table_top-. rim_width) >= norm_r
        )
      )

||
      (* S pocket, right wall, down -> left *)
      
      (by >= table_bot -. r) &&
      (
        (bx <= pocket_middle_x +.center_pocket_radius +. rim_width
         && bx >= pocket_middle_x -. center_pocket_radius-. rim_width) &&
        (
          bx -. (pocket_middle_x +. center_pocket_radius ) +. (by -. table_bot +. rim_width) >= norm_r
        )
      )

      
      
    ) ||
    (* regular wall bouncing, not near pockets *)
    (
      (bx >= table_right -.r && (true && by >= top_rim_offset +. rim_width)) ||
      (bx <= table_left +.r &&  (true && by >= top_rim_offset +. rim_width)) ||
      (by >= table_bot -.r &&
       (bx <= (pocket_right_x -. pocket_radius -. rim_width) && bx >= rim_width)
       &&
       (bx >= pocket_middle_x +. center_pocket_radius +. rim_width ||
        bx <= pocket_middle_x -.center_pocket_radius -. rim_width)
      ) ||
      (by <= table_top +.r &&
       (bx <= (pocket_right_x -. pocket_radius -. rim_width) && bx >= rim_width)
       &&
       (bx >= pocket_middle_x +. center_pocket_radius +. rim_width ||
        bx <= pocket_middle_x -. center_pocket_radius -. rim_width)
      )
    )

  then true
  else
    
    false
    
let bounce ball =
(* Algorithm comes fom http://www.petercollingridge.co.uk/pygame-physics-simulation/collisions *)
  let (x1, y1) = Ball.get_position ball in
  let r = Ball.get_radius ball in
  let norm_r = r /. 0.8 in
  (*if x1 <= 76. then*)
  
  if
    (* NE pocket, top wall, right -> up *)
    (x1 >= table_right -. Ball.get_radius ball
     && (y1 >= top_rim_offset -. r && y1 <= top_rim_offset +. rim_width) &&
     (y1-. (table_top +. top_rim_offset +. rim_width)) +.( x1 -. table_right +. 1.5 *. rim_width ) >=norm_r)
    ||
    (* NE pocket, top wall, up -> right *)
    (y1 <= table_top +. r &&
     (x1 >= (pocket_right_x -. pocket_radius -. rim_width) && x1 <= pocket_right_x -. pocket_radius) &&
     (y1 -. (pocket_right_x -. pocket_radius -. rim_width) +. (x1 -.table_top) <= norm_r)
    )
    ||
    (* SW pocket, top awll, left -> down *)
    (x1 <= table_left +. Ball.get_radius ball
     && (y1 <= bottom_rim_offset && y1 >= bottom_rim_offset -. rim_width) &&
     (y1-. ( bottom_rim_offset)) +.( x1 -. table_left +. rim_width ) <= norm_r )
    ||
    (* SW pocket, bottom wall, down -> left *)
    (y1 >= table_bot -. Ball.get_radius ball
     && (x1 <= rim_width +. 2. && x1 >= 0.) &&
     (y1 -. (table_bot -. 1.0 *. rim_width) +. (x1 -. table_left) >=0.)
    )
    (* N, left wall, up -> right*)
    ||
    (y1 <= table_top +. r) &&
    (
      (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
       && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
      (
        x1 -. (pocket_middle_x -. center_pocket_radius ) +. (y1 -. table_top) <= norm_r
      )
    )
    ||
    (* S pocket, right wall, down -> left *)
    (y1 >= table_bot -. r) &&
    (
      (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
       && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
      (
        x1 -. (pocket_middle_x +. center_pocket_radius ) +. (y1 -. table_bot+. rim_width) >= norm_r
      )
      )
    
     
  then 
    (*let norm_angle = atan2 0. 1. in
    let norm_vector = (1.,0.) in
    let tangent_angle = atan2 (-1.) 0. in
      let tan_vector = (0., -1.) in*)
    let norm_angle = atan2 ne_dy ne_dx in
    Firebug.console##log (string_of_float norm_angle);
    let norm_vector = (ne_dx,ne_dy) in
    let tangent_angle = atan2 (-.ne_dx) ne_dy in
    let tan_vector = (ne_dy, -.ne_dx) in
    
    let v1nb = -.dot (Ball.get_velocity ball) (norm_vector) in
    let v1t =  -.dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in
    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))
  else
  if
    (* NW pocket, top wall, up -> left *)
    (y1 <= table_top +. r &&
     (x1 >= 0. && x1 <= rim_width +. 2.) &&
     ((y1 +. rim_width) -. (x1) <= norm_r )
    )
    ||
    (* NW pocket, bottom wall, left -> up *)
    x1 <= table_left +. r &&
    (y1 >= top_rim_offset -. r&& y1 <= top_rim_offset +. rim_width) &&
    ((y1-. (table_top +. top_rim_offset +. rim_width)) +.( x1 -. table_left +. 1.5 *. rim_width ) >= norm_r)
    (* SE pocket, top wall, right -> down *)
    ||
    x1 >= table_right -. r &&
    (y1 <= bottom_rim_offset && y1 >= bottom_rim_offset -. rim_width) &&
    ((y1-. ( bottom_rim_offset)) +.( x1 -. table_right +. rim_width ) >= norm_r)
    (* SE pocket, bottom wall, down -> right *)
    ||
    y1 >= table_bot -. r &&
    (x1 >= (pocket_right_x -. pocket_radius -. rim_width) && x1 <= pocket_right_x -. pocket_radius) &&
    (x1 -. (pocket_right_x -. pocket_radius -. rim_width) +. (y1 -.table_bot) >= norm_r)
    (* N pocket, right wall, up -> left *)
    ||
    (y1 <= table_top +. r) &&
    (
      (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
       && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
      (
        x1 -. (pocket_middle_x +. center_pocket_radius ) -. (y1 -. table_top-. rim_width) >= norm_r
      )
    )
    (* S pocket, left wall, down -> right*)
    ||
    (y1 >= table_bot -. r) &&
    (
      (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
       && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) &&
      (
        x1 -. (pocket_middle_x -. center_pocket_radius) -. (y1 -. table_bot) <= norm_r
      )
    )
    
  then

    let norm_angle = atan2 nw_dy nw_dx in
    Firebug.console##log (string_of_float norm_angle);
    let norm_vector = (nw_dx,nw_dy) in
    let tangent_angle = atan2 (-.nw_dx) nw_dy in
    let tan_vector = (nw_dy, -.nw_dx) in

    let v1nb = -.dot (Ball.get_velocity ball) (norm_vector) in
    let v1t =  -.dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in
    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))
  else 
    
  if x1 <= table_left +. Ball.get_radius ball
  && y1 <= (bottom_rim_offset -. rim_width)
  && y1 >= top_rim_offset +. rim_width
  then 
    (*let norm_angle = atan2 0. 1. in
    let norm_vector = (1.,0.) in
    let tangent_angle = atan2 (-1.) 0. in
      let tan_vector = (0., -1.) in*)
    let norm_angle = atan2 left_dy left_dx in
    Firebug.console##log (string_of_float norm_angle);
    let norm_vector = (left_dx,left_dy) in
    let tangent_angle = atan2 (-.left_dx) left_dy in
    let tan_vector = (left_dy, -.left_dx) in
    
    let v1nb = dot (Ball.get_velocity ball) (norm_vector) in
    let v1t =  dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in

    (*Firebug.console##log (string_of_float v1nb);
    Firebug.console##log (string_of_float v1na);
    Firebug.console##log (string_of_float v1t);
    Firebug.console##log (string_of_float (v1ca|> fst));
      Firebug.console##log (string_of_float (v1ca|> snd));*)


    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))
  else
  if x1 >= table_right -. (Ball.get_radius ball)
  && y1 <= (bottom_rim_offset -. rim_width)
  && y1 >= top_rim_offset +. rim_width


  then
    (*let norm_angle = atan2 0. (-1.) in
    let norm_vector = (-1.,0.) in
    let tangent_angle = atan2 (1.) 0. in
      let tan_vector = (0., 1.) in *)
    let norm_angle = atan2 right_dy right_dx in
    let norm_vector = (right_dx,right_dy) in
    let tangent_angle = atan2 (left_dx) right_dy in
    let tan_vector = (right_dy, left_dx) in


    let v1nb = dot (Ball.get_velocity ball) (norm_vector) in
    let v1t = dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in
    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))
  else if y1 >= table_bot -. (Ball.get_radius ball)
       && x1 >= rim_width
       && x1 <= pocket_right_x -. pocket_radius -. rim_width
       && not (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
               && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) 

  then
    let norm_angle = atan2 bot_dy bot_dx in
    let norm_vector = (bot_dx,bot_dy) in
    let tangent_angle = atan2 (-.bot_dx) bot_dy in
    let tan_vector = (bot_dy, -.bot_dx) in

    let v1nb = dot (Ball.get_velocity ball) (norm_vector) in
    let v1t = dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in
    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))
  else if y1 <= table_top +. (Ball.get_radius ball)
       && x1 >= rim_width
       && x1 <= pocket_right_x -. pocket_radius -. rim_width
       && not (x1 <= pocket_middle_x +. center_pocket_radius +. rim_width
               && x1 >= pocket_middle_x -. center_pocket_radius -. rim_width) 

  then

    let norm_angle = atan2 top_dy top_dx in
    let norm_vector = (top_dx,top_dy) in
    let tangent_angle = atan2 (-.top_dx) top_dy in
    let tan_vector = (top_dy, -.top_dx) in
    let v1nb = dot (Ball.get_velocity ball) (norm_vector) in
    let v1t = dot (Ball.get_velocity ball) tan_vector in
    let v1na = -. v1nb in
    let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
                v1t *. sin tangent_angle +. v1na *. sin norm_angle
               ) in
    (Ball.get_id ball,  (diff v1ca (Ball.get_velocity ball)))

  else
    (Ball.get_id ball,  (0.0, 0.0))
  
  

let compute_collisions (ball_list: Ball.t list) =
  let rec collision_h2 ball_list ball (cur_index:int) (max_index:int) (acc) =
    match ball_list with
    | [] -> acc
    | h::t ->
      (*printf "max_index: %d\n" max_index;
        printf "cur_index %d\n" cur_index;*)
            
      if cur_index >= max_index then
        (*let _ = printf "short-circuit:\n" in*)
      acc
      else
      if is_overlap ball h then
        begin match collide h ball with
          | ((h_id', h_diff), (ball_id', ball_diff)) ->
            (*Ball.print_ball h';
            Ball.print_ball h;
            Ball.print_ball ball';
              Ball.print_ball ball;*)
            let new_acc = 
              ((update h_id' h_diff (fst acc)) |> (update ball_id' ball_diff),
               Hit(h)::Hit(ball)::(snd acc)
              )
            in
              collision_h2 t ball (cur_index+1) max_index new_acc
        end
      else
        collision_h2 t ball (cur_index+1) max_index acc
  in
  let rec collision_h orig_ball_list ball_list cur_index acc=
    match ball_list with
    | [] -> acc
    | h::t ->
      (* Handle wall collisions here, once *)
      if is_bounce h then
        begin match bounce h with
          | (h_id', h_diff) ->
            let new_acc = ((update h_id' h_diff (fst acc)), Collide(h)::(snd acc)) in
            collision_h orig_ball_list t (cur_index+1) new_acc
        end
      else
      let new_acc = (collision_h2 orig_ball_list h 0 cur_index acc) in
      collision_h orig_ball_list t (cur_index+1) new_acc


  in
  let ball_v_diffs = (collision_h ball_list ball_list 0 (IntMap.empty, [])) in
  (*let new_balls = (collision_h ball_list ball_list 0 init_balls) in*)
  let new_balls = List.fold_left (fun (acc: Ball.t list) x ->
      if IntMap.mem (Ball.get_id x) (fst ball_v_diffs)
      then
        let old_v = Ball.get_velocity x in
        let v_diff = IntMap.find (Ball.get_id x) (fst ball_v_diffs) in
        let new_v = sum old_v v_diff in
        let new_ball = Ball.change_velocity x new_v in
        new_ball::acc
      else
        x::acc
    ) [] ball_list
  in

  (new_balls, snd ball_v_diffs)
  (*IntMap.fold (fun k v acc -> v::acc) new_balls []*)

let is_converged ball_list =
  List.fold_left (fun acc x ->
      (norm (Ball.get_velocity x) < 0.001) && acc
    ) true ball_list




let simulate_timestep ball_list ts : (Ball.t list * event list)=
  (* First, we move everything at once.
     Then, handle all collisions, adding the new velocities to an accumulator.
     Then, shift everybody who collided by their velocity * ts...or something.
     Return (updated list, list of interesting events this timestep)
  *)

  let moved_ball_list =
    List.fold_left (fun acc x ->
        (Ball.update_position x ts |> (apply_friction ts))::acc
        (*(Ball.update_position x ts)::acc*)
    ) [] ball_list
  in
  let collision_ball_list =
    compute_collisions moved_ball_list
    (*moved_ball_list*)
  in
  let moved_ball_list2 =
    List.fold_left (fun acc x ->
        (Ball.update_position x ts |> (apply_friction ts))::acc
    ) [] (fst collision_ball_list)
  in


  (moved_ball_list2, snd collision_ball_list)
