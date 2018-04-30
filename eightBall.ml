include Data



(*Assumed implementation of state of eight ball, should be
imported from another class*)
type state = {
  player: player; (*current player this turn*)
  color: color; (*current player color*)
  break: boolean; (*whether the game just started and the table is open*)
  scratch: boolean; (*whether the player fouled*)
  game_over: boolean; (*whether the game ended*)
}

(*Assumed implementation of state of eight ball, should be
imported from another class*)
type event =
| None (* Done *)
| Hit of ball (* Cue ball contacts another ball*)
| Sink of ball (* A ball sinks, the cue ball does not have to have contacted it*)

let resolve_hit (s:state) (b:ball) : state = {
  player = s.player; (*player does not change*)
  color = s.color; (*player does not change*)
  break = s.break; (*player does not change*)
  scratch = s.scratch; (*if the first ball is not your own ball*)
  game_over = s.game_over; (*player does not change*)
}


(*returns the next state of the game
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let rec next_state (is:state) (el:event list) : state =
  match el with
  | [] -> is
  | h :: t -> match h with
              | None -> next_state is t
              | Hit b -> next_state (resolve_hit is b) t
              | Sink b -> next_state (resolve_sink is b) t
