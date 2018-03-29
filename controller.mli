(* A [Controller] processes a turn and updates a game state *)
module type Controller = sig

  (* [state] is an abstract type representing the current game state *)
  type state

  (* [turn] is an abstract type representing a turn from the user *)
  type turn

  (* [state'] returns the new game state after applying the turn. This game state
   * encapsulates all the updated positions of balls and the scores associated
   * with any balls that were sunk by the turn *)
  val state' : state -> turn -> state

end