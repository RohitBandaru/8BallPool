(*The possible types of balls*)
type b_type  =
  | Cue
  | Solid
  | Stripe
  | Black

(* A Ball stores information about a given ball. *)
(* t represents an instance of a ball *)
type t

(* [get_mass t] returns the mass of t *)
val get_mass: t -> float

(* [get_id t] returns the unique id that identifies this ball *)
val get_id: t -> int

(* [get_name t] returns the friendly name of the ball *)
val get_name: t -> string

(* [get_type t] returns the type of the ball *)
val get_type: t -> b_type

(* [get_color t] returns the color of the ball as a rgb string*)
val get_color: t -> string

(* [get_momentum t] returns a vector representing the momentum of the ball *)
val get_momentum: t -> float * float

(* [get_velocity t] returns a vector representing the velocity of the ball *)
val get_velocity: t -> float * float

(* [get_position t] returns a vector representing the position of the ball *)
val get_position: t -> float * float

(* [get_radius t] returns the radius of the ball t *)
val get_radius: t -> float

(* [change_velocity t v] returns a new ball with all aspects the same but
   with velocity set to [v] *)
val change_velocity: t -> float * float -> t

(* [change_position t p] returns a new ball with all aspects the same but
   with position set to [p] *)
val change_position: t -> float * float -> t

val update_position: t -> float -> t

(* [create_ball name id group color velocity position mass radius] creates a new ball*)
val create_ball: string -> int -> b_type -> string -> float * float -> float * float -> float -> float -> t

(* [compare t1 t2] is a total ordering on balls using their unique ids *)
val compare: t -> t -> int

(* [print_ball b] prints out information about the ball *)
val print_ball: t -> unit
