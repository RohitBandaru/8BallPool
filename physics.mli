open Ball
open Data

(* [is_overlap t1 t2] returns True iff t1 and t2 are overlapping each other
   ie. there is a point x s.t. the distance from x to t1's center <= t1's radius
   and the distance from x to t2's center <= t2's radius *)

val is_overlap: Ball.t -> Ball.t -> bool


(* [collide t1 t2] returns a tuple (t1', t2') representing the states of t1, t2
after they elastically collide
 * requires: is_overlap t1 t2 returns True
*)
(*val collide: Ball.t -> Ball.t -> Ball.t * Ball.t*)
val collide: Ball.t -> Ball.t -> ((int * (float * float)) * (int * (float * float)))

val simulate_timestep: Ball.t list -> float ->  (Ball.t list * event list)

val compute_collisions: Ball.t list -> Ball.t list
