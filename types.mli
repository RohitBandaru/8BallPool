
(* A Ball stores information about a given ball. *)
module type Ball = sig
  (* t represents an instance of a ball *)
  type t

  (* [get_mass t] returns the mass of t *)
  val get_mass: t -> float

  (* [get_id t] returns the unique id that identifies this ball *)
  val get_id: t -> string

  (* [get_name t] returns the friendly name of the ball *)
  val get_name: t -> string
end
