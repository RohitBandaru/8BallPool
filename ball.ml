open Printf

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
let compare t1 t2 =
  Pervasives.compare t1.id t2.id

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

let change_position b p = {b with position = p}

let update_position b ts =
  let new_pos = match b.position, b.velocity with
    | (x, y), (vx, vy) ->
      if b.id = 0 then
        ()
        (*Firebug.console##log (string_of_float x)*)
      else ()
      ;
      (x +. vx *. ts, y +. vy *. ts)
  in
  {b with position = new_pos}
  
let create_ball name id group color velocity position mass radius =
  {
    name; id; group; color; velocity; position; mass; radius
  }

let print_ball b =
  printf "name: %s\n id: %d\n color: %s\n velocity: (%f, %f)\n position: (%f, %f)\n"
    b.name b.id b.color (b.velocity |> fst) (b.velocity |> snd)
    (b.position |> fst) (b.position |> snd)
