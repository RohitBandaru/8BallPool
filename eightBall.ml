include Data

type status =
  |Playing
  |Won
  |Lost

(*Assumed implementation of state of eight ball, should be
  imported from another class*)
type player = {
  id: int; (*current player this turn*)
  group: b_type; (*stripes or solids*)
  (*List of balls left to sink other than the 8 ball.
    Contains all balls except cue and 8 if break = true*)
  balls_left : ball list;
  status : status; (**)
}

(*Assumed implementation of gamestate of eight ball, should be
imported from another class*)
type state = {
  player: player; (*current player this turn*)
  other_player: player; (*the other player, which is the current player if 1p mode*)
  break: boolean; (*whether the game just started and the table is open*)
  scratch: boolean; (*whether the player fouled*)
  continue: boolean; (*Whether the player just sunk a ball of his type*)
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
      continue = s.continue; (*hit does not change continue*)
      game_over = s.game_over; (*player does not change*)
    }

(*remove a ball from the list of balls*)
let remove_ball (b : ball) (l: ball list) : ball list =
  List.filter (fun x -> x.id <> b.id) l

(*resolve the ball sinking in a pocket
  Only called for first ball hit each turn*)
let resolve_sink (s:state) (b:ball) : state =
  if (b.group = Cue)
  then {
    player = s.player; (*player does not change*)
    other_player = s.other_player; (*other player doesn't change*)
    break = s.break; (*player does not change*)
    scratch = true; (*if the first ball is not your own ball, scratch*)
    continue = s.continue; (*-SCRATCH OVERIDES CONTINUE-*)
    game_over = s.game_over; (*player does not change*)
  }
  else if (b.group = Black)
  then
    let p = s.player in {
      player =
        if (p.balls_left = [])
        then {
          id = p.id;
          group = p.group;
          balls_left = p.balls_left;
          status = Won;
        } else {
          id = p.id;
          group = p.group;
          balls_left = p.balls_left;
          status = Lost;
        };
    other_player =
      if (other_player <> player)
      then
        if (p.balls_left = [])
        then {
          id = p.id;
          group = p.group;
          balls_left = p.balls_left;
          status = Lost;
        } else {
          id = p.id;
          group = p.group;
          balls_left = p.balls_left;
          status = Won;
        }
      else p.other_player; (*TODO decide*)
    break = s.break; (*break does not change*)
    scratch = s.scratch; (*scratch does not change*)
    continue = s.continue; (*continue does not change*)
    game_over = true; (*player does not change*)
    }
  else if (s.break) (*if player did not yet decide ball type*)
  then {
    player =
      let p = s.player in {
        id = p.id;
        group = p.group;
        balls_left =
          remove_ball b
            (List.filter (fun x -> x.group = b.group) p.balls_left);
        status = p.status;
      }; (*player just picked a side*)
    other_player =
      let p = s.other_player in {
        id = p.id;
        group = p.group;
        balls_left =
          remove_ball b
            (List.filter (fun x -> x.group = b.group) p.balls_left);
        status = p.status;
      }
    break = false; (*player just sunk valid ball*)
    scratch = s.scratch; (*no scratch if hit own ball*)
    continue = true; (*continue not decided until ball sinks*)
    game_over = s.game_over; (*player does not change*)
  }
  else if (b.group = s.player.group)
  then { (*sink own ball*)
    player =
      let p = s.player in {
        id = p.id;
        group = p.group;
        balls_left = remove_ball b p.balls_left; (*1 less ball*)
        status = p.status;
      };
    other_player = s.other_player; (*other player doesn't change*)
    break = s.break; (*player does not change*)
    scratch = s.scratch; (*if the first ball is not your own ball, scratch*)
    continue = true; (*sunk a good ball!*)
    game_over = s.game_over; (*player does not change*)
  }
  else (*must be sink ball of other person*){

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
              | Sink b -> if (b.group = Cue) then
                (*literally scratch is the only possibility*)
                {
                  player = next player s.player; (*next player's turn*)
                  other_player = s.other_player; (*other player doesn't change*)
                  break = s.break; (*TODO does break continue in scratch*)
                  scratch = true; (*no balls hit, *)
                  continue = s.continue; (*continue does not change*)
                  game_over = s.game_over; (*game_over does not change*)
                }
                else failwith "Impossible to sink without hitting first"
