type b_type  =
  | Cue
  | Solid
  | Stripe
  | Black

type t = {name: string;
          id: int;
          group: b_type;
          color: string;
          velocity: float * float;
          position: float * float;
          mass: float;
          radius: float;}

let get_mass t = t.mass

let get_id t = t.id

let get_name t = t.name

let get_type t = t.group

let get_color t = t.color

let get_momentum t =
  match t.velocity with
  | (vx, vy) -> (vx *. t.mass, vy *. t.mass)

let get_velocity t = t.velocity

let get_radius t = t.radius

let get_position t = t.position

let change_velocity b v = {b with velocity = v}

let create_ball name id group color velocity position mass radius =
  {
    name; id; group; color; velocity; position; mass; radius
  }
