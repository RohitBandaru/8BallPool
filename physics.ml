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

  let (new_v) = (old_vx -. old_vx *. ts *. 0.2, old_vy -. old_vy *. ts *. 0.2) in


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
  Firebug.console##log (string_of_float v1na);
  Firebug.console##log (string_of_float v2na);
  Firebug.console##log (string_of_float v1t);
  Firebug.console##log (string_of_float v2t);
  Firebug.console##log (string_of_float (norm_vector |> fst));
  Firebug.console##log (string_of_float (norm_vector |> snd));


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


let compute_collisions (ball_list: Ball.t list) =
  let rec collision_h2 ball_list ball (cur_index:int)
      (max_index:int) (acc) =
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
              (*(IntMap.add (Ball.get_id h') h' acc)
                |> IntMap.add (Ball.get_id ball') ball'*)
              (update h_id' h_diff acc) |> (update ball_id' ball_diff)
            in
            (*(BallSet.remove h acc) |> (BallSet.remove ball)
              |> (BallSet.add h') |> (BallSet.add ball')*)
              collision_h2 t ball (cur_index+1) max_index new_acc
        end
      else
        collision_h2 t ball (cur_index+1) max_index acc
  in
  let rec collision_h orig_ball_list ball_list cur_index acc=
    match ball_list with
    | [] -> acc
    | h::t ->
      let new_acc = (collision_h2 orig_ball_list h 0 cur_index acc) in
      collision_h orig_ball_list t (cur_index+1) new_acc


  in
  (*
  let init_balls = List.fold_left (fun acc x ->
      IntMap.add (Ball.get_id x) x acc
    ) IntMap.empty ball_list
  in
*)
  let ball_v_diffs = (collision_h ball_list ball_list 0 IntMap.empty) in
  (*let new_balls = (collision_h ball_list ball_list 0 init_balls) in*)
  let new_balls = List.fold_left (fun (acc: Ball.t list) x ->
      if IntMap.mem (Ball.get_id x) ball_v_diffs
      then
        let old_v = Ball.get_velocity x in
        let v_diff = IntMap.find (Ball.get_id x) ball_v_diffs in
        let new_v = sum old_v v_diff in
        let new_ball = Ball.change_velocity x new_v in
        new_ball::acc
      else
        x::acc
    ) [] ball_list
  in

  new_balls
  (*IntMap.fold (fun k v acc -> v::acc) new_balls []*)





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
    ) [] collision_ball_list
  in


  (moved_ball_list2, [])
