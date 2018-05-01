open Ball
    
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

let norm v1 = (dot v1 v1) ** 0.5

let collide t1 t2 =
  (* Algorithm comes fom http://www.petercollingridge.co.uk/pygame-physics-simulation/collisions *)
  let (x1, y1) = Ball.get_position t1 in
  let (x2, y2) = Ball.get_position t2 in
  let dy, dx = (y1 -. y2, x1 -. x2) in
  let tangent_angle = atan2 dy dx in
  let norm_angle = 90.0 -. tangent_angle in
  (* We only care about the velocity perpendicular to the tangent *)
  let norm_vector = (dx, dy) in
  let norm_vector = (dx /. norm norm_vector, dy /. norm norm_vector) in
  let tan_vector = (-.dy /. norm norm_vector, dx /. norm norm_vector) in
  
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
              v1t *. sin tangent_angle +. v1na *. sin tangent_angle)
            
  in
  (* Velocity of ball 2, cartesian coordinates, after *)
  let v2ca = (v2t *. cos tangent_angle +. v2na *. cos norm_angle,
              v2t *. sin tangent_angle +. v2na *. sin tangent_angle)
  in
  (Ball.change_velocity t1 v1ca, Ball.change_velocity t2 v2ca)


  
