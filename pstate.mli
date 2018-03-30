
(* [p_state] is an abstract type representing the state of a physics object *)
type p_state    

(* [create_state m v] creates an instance of p_state with the given mass and
   velocity vector.
 * requires: [m > 0]
 *)
val create_state: float -> (float * float)

(* [get_x s] returns a tuple of the position of the object described by [s]
 * in the form (x_x, x_y) where x_x is the x location and x_y is the y momentum
 *)
val get_x: p_state -> (float * float)                      

(* [get_p s] returns a tuple of the momentum of the object described by [s]
 * in the form (v_x, v_y) where v_x is the x momentum and v_y is the y momentum
 *)
val get_p: p_state -> (float * float)

(* [get_m s] returns the current mass of the object *)
val get_m: p_state -> float

(* [get_v s] returns a tuple of the velocity of the object described by [s]
 * in the form (v_x, v_y) where v_x is the x velocity and v_y is the y velocity
 *)
val get_v: p_state -> (float * float)

(* [is_fixed s] returns true if the object described by s does not have its own
   parameters changed in a collision, and false otherwise *)
val is_fixed: p_state -> bool

(* [collide (s1, s2)] returns (s1', s2') where s1' and s2' are the corresponding
   states after collision of s1 and s2 *)
val collide: (p_state * p_state) -> (p_state * p_state)

(* [step s t] returns the new state after evolving [s] for a time [t] *)
val step: p_state -> float -> p_state

(* [overlap (s1, s2)] returns true iff [s1], [s2] are close enough to collide *)
val overlap: (p_state * p_state) -> bool

(* [converged s] returns true iff none of the objecs in [s] are moving *)
val converged: p_state -> bool
