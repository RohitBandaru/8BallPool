include Data

(*Assumed implementation of state of eight ball, should be
  imported from another class*)
type player = {
  id: int; (*current player this turn*)
  color: b_color; (*current player color*)
}

(*Assumed implementation of gamestate of eight ball, should be
imported from another class*)
type state = {
  player: player; (*current player this turn*)
  other_player: player; (*the other player*)
  break: boolean; (*whether the game just started and the table is open*)
  scratch: boolean; (*whether the player fouled*)
  continue: boolean; (*Whether the player just sunk a ball of his color*)
  game_over: boolean; (*whether the game ended*)
}

(*Assumed implementation of state of eight ball, should be
imported from another class*)
type event =
| None (* Done *)
| Hit of ball (* Cue ball contacts another ball*)
| Sink of ball (* A ball sinks, the cue ball does not have to have contacted it*)

(*get the next player to take a turn given the state and current player*)
let next_player (s:state) (p:player) : player =
  let pl = s.player_list in
  if (s.scratch)
  then List.

(*resolve the cue ball hitting another ball*)
let resolve_hit (s:state) (b:ball) : state = {
  player = s.player; (*player does not change*)
  player_list = s.player_list; (*player list never changes*)
  break = s.break; (*player does not change*)
  scratch = s.scratch; (*if the first ball is not your own ball*)
  game_over = s.game_over; (*player does not change*)
}

(*returns the next state of the game
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let rec step_state (is:state) (el:event list) : state =
  match el with
  | [] -> {
      player = next player s.player; (*next player's turn*)
      player_list = s.player_list; (*player list never changes*)
      break = s.break; (*player does not change*)
      scratch = s.scratch; (*TODO if the first ball is not your own ball*)
      continue = s.continue; (*TODO*)
      game_over = s.game_over; (*game_over does not change*)
    }
  | h :: t -> match h with
              | None | Hit _ -> (*hitting other colored balls no longer matters*)
                      step_state is t
              | Sink b -> step_state (resolve_sink is b) t

(*takes the first step in calculating the next state
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let next_state (is:state) (el:event list) : state =
  match el with
    | [] -> {
        player = next player s.player; (*next player's turn*)
        player_list = s.player_list; (*player list never changes*)
        break = s.break; (*player does not change*)
        scratch = true; (*no balls hit*)
        continue = s.continue; (*continue does not change*)
        game_over = s.game_over; (*game_over does not change*)
      }
  | h :: t -> match h with
              | None -> step_state is t
              | Hit b -> step_state (resolve_hit is b) t
              | Sink b -> if (b.id = "cue") then
                (*literally scratch is the only possibility*)
                {
                  player = next player s.player; (*next player's turn*)
                  player_list = s.player_list; (*player list never changes*)                  color = s.color; (*player does not change*)
                  break = s.break; (*TODO does break continue in scratch*)
                  scratch = true; (*no balls hit, *)
                  continue = s.continue; (*continue does not change*)
                  game_over = s.game_over; (*game_over does not change*)
                }
                else failwith "Impossible to sink without hitting first"
