
(* [p_state] is an abstract type representing the state of a physics object *)
type p_state    

(* [create_state m v] creates an instance of p_state with the given mass and
   velocity vector.
 * requires: [m > 0]
 *)
val create_state: float -> (float * float)

(* [get_p s] returns a tuple of the momentum of the object described by [s]
 * in the form (p_x, p_y) where p_x is the x momentum and p_y is the y momentum
 *)
val get_p: p_state -> (float * float)

(* [get_m s] returns the current mass of the object *)
val get_m: p_state -> float

(* [get_p s] returns a tuple of the momentum of the object described by [s]
 * in the form (v_x, v_y) where v_x is the x momentum and v_y is the y momentum
 *)
val get_v: p_state -> (float * float)

(* [is_fixed s] returns true if the object described by s does not have its own
   parameters changed in a collision, and false otherwise *)
val is_fixed: p_state -> bool
