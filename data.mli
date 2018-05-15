open Ball

type ball = t

(* [game_type] is the game mode default Eight-ball *)
type game_type =
  |EightBall

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
  ob: int; (*the id of the current object ball (first ball hit that turn)*)
  collide: bool; (*whether the object ball or cue ball hit a rail*)
  scratch: bool; (*whether the player fouled*)
  continue: bool; (*Whether the player just sunk a ball of his type*)
  game_over: bool; (*whether the game ended*)
}

(* [state] is a type representing the game state. *)
type state = logic_state * (ball list)

(*[event] is the game logic actions that can occur in a game*)
type event =
  | Collide of ball(* A ball collides with pool table wall *)
  | Hit of ball (* Cue ball contacts another ball*)
  | Sink of ball (* A ball sinks, the cue ball does not have to have contacted it*)

(* [init_state s] is the initial state of the pool game given a game_type
	It will initialize a game state taking into account different variables
	such as number of balls, game rules, cue type etc. *)
val init_state : game_type -> state

(* [get_logic] returns the logic state of a game state type*)
val get_logic : state -> logic_state

val get_balls : state -> ball list

val get_winner_id : state -> int

val search_ball : state -> int -> ball

val update_cue_ball_position : state ->  float*float -> state
(* [ball_locations s] is a list of pool balls and their corresponding locations
 at a given game state. *)
val ball_locations : state -> ball list

val eight_ball_init_ball_pos : ball list

