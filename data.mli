(* [state] is an abstract type representing the game state. *)
type state

(* [player] is an abstract type representing a player in the game *)
type player

(* [game_type] is the game mode default Eight-ball *)
type game_type = EightBall

(* [location] is a variant type representing the location of a pool ball *)
type b_location =
	| Sunk
	| Table of (float * float)

(* [ball] is a record type representing of a pool ball *)

type b_type  =
	| Cue
	| Solid of int
  | Stripe of int
  | Black

type b_color = Red | Blue | Green

type ball = {
	id 				: int;
	group			: b_type ;
	color 		: b_color;
	location  : b_location;
	name 		: string;
	velocity 	: float * float;
	}

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
val ball_locations : state -> (ball * b_location)

(* [cue_location s] is the current location of the cue ball *)
val cue_location : state -> b_location

(* [next s] is the next move that will take place *)
val move : state -> move
