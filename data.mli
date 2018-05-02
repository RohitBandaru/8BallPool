open Ball

(* [game_type] is the game mode default Eight-ball *)
type game_type =
  |EightBallSolo
  |EightBallTwoP

(*[status] is a type representing the state of the player, whether
they are currently playing, won, or lost the game. *)
type status =
  |Playing
  |Won
  |Lost

(* [player] is a type representing a player in the game *)
type player = {
  id: int; (*current player this turn*)
  group: b_type;(*stripes or solids*)
  (*List of balls left to sink other than the 8 ball.
    Contains all balls except cue and 8 if break = true
  Stored as ball id by ball type*)
  balls_left : (int * b_type) list;
  status : status; (* playing, won lost*)
}

(* [logic_state] is a type representing the game's logical state. *)
type logic_state = {
  player: player; (*current player this turn*)
  other_player: player; (*the other player, which is the current player if 1p mode*)
  break: bool; (*whether the game just started and the table is open*)
  scratch: bool; (*whether the player fouled*)
  continue: bool; (*Whether the player just sunk a ball of his type*)
  game_over: bool; (*whether the game ended*)
}

(* [state] is a type representing the game state. *)
type state = logic_state * (ball list)

(*[event] is the type*)
type event =
  | None (* Done *)
  | Hit of ball (* Cue ball contacts another ball*)
  | Sink of ball (* A ball sinks, the cue ball does not have to have contacted it*)

(* [move] representing *)
type move =
	| None
	| Move of { player:player; velocity:(float*float) }
	| Place of b_location

(* [init_state s] is the initial state of the pool game given a game_type
	It will initialize a game state taking into account different variables
	such as number of balls, game rules, cue type etc. *)
val init_state : game_type -> state

(* [ball_locations s] is a list of pool balls and their corresponding locations
 at a given game state. *)
val ball_locations : state -> ball list

(* [next s] is the next move that will take place *)
val move : state -> move
