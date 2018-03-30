module type Game = sig

  (* [inputParams] is an abstract type representing the game type and init state *)
  type inputParams

  (* [play_game] starts the game specified by the input parameters *)
  val play_game : inputParams -> unit

  (* [main] starts the GUI, which updates as the game progresses *)
  val main : unit -> unit

end