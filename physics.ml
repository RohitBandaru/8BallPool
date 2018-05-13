open Ball
open Printf
open Data

type ball = Ball.t

let is_overlap t1 t2 =
  let (x1, y1) = Ball.get_position t1 in
  let (x2, y2) = Ball.get_position t2 in
  let r1 = Ball.get_radius t1 in
  let r2 = Ball.get_radius t2 in
  (x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0 < (r1 +. r2) ** 2.0

let dot v1 v2 =
  match v1, v2 with
  | ((v1x, v1y), (v2x, v2y)) -> v1x *. v2x +. v1y *. v2y

(* diff (a1, a2), (b1, b2) is (a1-b1, a2-b2) *)
let diff (a1,a2) (b1,b2) = (a1-.b1, a2-.b2)

let norm v1 = (dot v1 v1) ** 0.5

let apply_friction ts b=
  let (old_vx, old_vy) = Ball.get_velocity b in
  let (new_v) = (old_vx -. old_vx *. ts *. 0.1, old_vy -. old_vy *. ts *. 0.1) in
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
  let norm_vector = (dx /. norm norm_vector, dy /. norm norm_vector) in
  let tan_vector = (-.dy /. norm norm_vector, dx /. norm norm_vector) in
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
  let v1na = (v1nb *. (m1 -. m2) +. 2.0 *. m2 *. v2nb) /. (m1 +. m2) in
  (* Velocity of ball 2, normal coordinates, after *)
  let v2na = (v2nb *. (m2 -. m1) +. 2.0 *. m1 *. v1nb) /. (m1 +. m2) in
  (* Velocity of ball 1, cartesian coordinates, after *)
  let v1ca = (v1t *. cos tangent_angle +. v1na *. cos norm_angle,
              v1t *. sin tangent_angle +. v1na *. sin norm_angle)

  in
  (* Velocity of ball 2, cartesian coordinates, after *)
  let v2ca = (v2t *. cos tangent_angle +. v2na *. cos norm_angle,
              v2t *. sin tangent_angle +. v2na *. sin norm_angle)
  in


  (* (diff (Ball.get_velocity t1) v1ca, diff (Ball.get_velocity t2) v2ca)*)
   (Ball.change_velocity t1 v1ca, Ball.change_velocity t2 v2ca)

module BallSet = Set.Make(Ball)

let compute_collisions (ball_list: Ball.t list) =
  let rec collision_h2 ball_list ball (cur_index:int)
      (max_index:int) (acc) =
    match ball_list with
    | [] -> acc
    | h::t ->
      if cur_index >= max_index then acc
      else
      if is_overlap ball h then
        begin match collide h ball with
          | (h', ball') ->
            (BallSet.remove h acc) |> (BallSet.remove ball)
            |> (BallSet.add h') |> (BallSet.add ball')
        end
      else
        collision_h2 t ball (cur_index+1) max_index acc
  in
  let rec collision_h ball_list cur_index acc=
    match ball_list with
    | [] -> acc
    | h::t -> collision_h t (cur_index+1)
                (collision_h2 ball_list h 0 cur_index acc)

  in
  BallSet.elements (collision_h ball_list 0 (BallSet.of_list ball_list))



let simulate_timestep ball_list ts : (Ball.t list * event list)=
  (* First, we move everything at once.
     Then, handle all collisions, adding the new velocities to an accumulator.
     Then, shift everybody who collided by their velocity * ts...or something.
     Return (updated list, list of interesting events this timestep)
  *)

  let moved_ball_list =
    List.fold_left (fun acc x ->
      (Ball.update_position x ts |> (apply_friction ts))::acc
    ) [] ball_list
  in
  let collision_ball_list =
    compute_collisions moved_ball_list
  in

  (collision_ball_list, [])
