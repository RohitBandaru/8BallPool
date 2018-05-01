include Data

(*Assumed implementation of state of eight ball, should be
  imported from another class*)
type player = {
  id: int; (*current player this turn*)
  color: b_color; (*stripes or solids*)
  (*list of balls left to sink. contains all balls except cue and 8 if break = true*)
  balls_left : ball list;
}

(*Assumed implementation of gamestate of eight ball, should be
imported from another class*)
type state = {
  player: player; (*current player this turn*)
  other_player: player; (*the other player, which is the current player if 1p mode*)
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
let next_turn (s:state) : state =
  (* if (s.scratch)
  then {
    player = s.other_player; (*players change*)
    other_player = s.player; (*players change*)
    break = s.break; (*player does not change*)
    scratch = s.scratch; (*if the first ball is not your own ball*)
    continue = false; (*next player has to sink their balls to continue*)
    game_over = s.game_over; (*player does not change*)
  }
  else if (s.break)
  then {
    player = s.other_player; (*players change*)
    other_player = s.player; (*players change*)
    break = s.break; (*player does not change*)
    scratch = s.scratch; (*if the first ball is not your own ball*)
    continue = false; (*next player has to sink their balls to continue*)
    game_over = s.game_over; (*player does not change*)
  } *)
  if (s.scratch || not s.continue)
  then {
    player = s.other_player; (*players change*)
    other_player = s.player; (*players change*)
    break = s.break; (*player does not change*)
    scratch = s.scratch; (*if the first ball is not your own ball*)
    continue = false; (*next player has to sink their balls to continue*)
    game_over = s.game_over; (*player does not change*)
  }
  else s

  (*resolve the cue ball hitting another ball
    Only called for first ball hit each turn*)
  let resolve_hit (s:state) (b:ball) : state =
    if (s.break) then s (*if in break hitting any ball is fine*)
    else if (*if ball hit is in legal balls to hit*)
      (List.exists (fun x -> x = b) s.player.balls_left)
    then {
      player = s.player; (*player does not change*)
      other_player = s.other_player; (*other player doesn't change*)
      break = s.break; (*player does not change*)
      scratch = false; (*no scratch if hit own ball*)
      continue = s.continue; (*continue not decided until ball sinks*)
      game_over = s.game_over; (*player does not change*)
    } else {
      player = s.player; (*player does not change*)
      other_player = s.other_player; (*other player doesn't change*)
      break = s.break; (*player does not change*)
      scratch = true; (*if the first ball is not your own ball, scratch*)
      continue = s.continue; (*TODO*)
      game_over = s.game_over; (*player does not change*)
    }

(*resolve the ball sinking in a pocket
  Only called for first ball hit each turn*)
let resolve_sink (s:state) (b:ball) : state =
  if (s.break) then s (*if in break hitting any ball is fine*)
  else if (*if ball hit is in legal balls to hit*)
    (List.exists (fun x -> x = b) s.player.balls_left)
  then {
    player = s.player; (*player does not change*)
    other_player = s.other_player; (*other player doesn't change*)
    break = s.break; (*player does not change*)
    scratch = false; (*no scratch if hit own ball*)
    continue = s.continue; (*continue not decided until ball sinks*)
    game_over = s.game_over; (*player does not change*)
  } else {
    player = s.player; (*player does not change*)
    other_player = s.other_player; (*other player doesn't change*)
    break = s.break; (*player does not change*)
    scratch = true; (*if the first ball is not your own ball, scratch*)
    continue = s.continue; (*TODO*)
    game_over = s.game_over; (*player does not change*)
  }

(*returns the next state of the game
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let rec step_state (s:state) (el:event list) : state =
  match el with
  | [] -> next_turn s
  | h :: t -> match h with
              | None | Hit _ -> (*hitting other colored balls no longer matters*)
                      step_state s t
              | Sink b -> step_state (resolve_sink s b) t

(*takes the first step in calculating the next state
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let next_state (is:state) (el:event list) : state =
  match el with
    | [] -> (*scratch if no balls hit*) {
        player = next player s.player; (*next player's turn*)
        other_player = s.other_player; (*other player doesn't change*)
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
                  other_player = s.other_player; (*other player doesn't change*)                 color = s.color; (*player does not change*)
                  break = s.break; (*TODO does break continue in scratch*)
                  scratch = true; (*no balls hit, *)
                  continue = s.continue; (*continue does not change*)
                  game_over = s.game_over; (*game_over does not change*)
                }
                else failwith "Impossible to sink without hitting first"
