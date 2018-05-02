
(* A Ball stores information about a given ball. *)
(* t represents an instance of a ball *)
type t

(* [get_mass t] returns the mass of t *)
val get_mass: t -> float

(* [get_id t] returns the unique id that identifies this ball *)
val get_id: t -> string

(* [get_name t] returns the friendly name of the ball *)
val get_name: t -> string

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

val create_ball: string -> string -> float * float -> float * float -> float -> float -> t
  
