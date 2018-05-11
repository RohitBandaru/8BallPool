open Data
open Ball

(*get the next player to take a turn given the state and current player*)
let next_turn (s:logic_state) : logic_state =
  if (s.other_player.id = s.player.id)
  then s
  else if (s.scratch || not s.continue)
  then {s with player = s.other_player;
               other_player = s.player;
       continue = false;}
  else s

  (*resolve the cue ball hitting another ball
    Only called for first ball hit each turn*)
  let resolve_hit (s:logic_state) (b:ball) : logic_state =
    if (s.break) then s (*if in break hitting any ball is fine*)
    else if (*if ball hit is in legal balls to hit*)
      (List.exists (fun x -> fst x = get_id b) s.player.balls_left)
    then {s with scratch = false;}
    else {s with scratch = true;}

(*remove a ball from the list of balls*)
let remove_ball (b : ball) (l: (int * b_type) list) : (int * b_type) list =
  List.filter (fun x -> fst x <> get_id b) l

let opposite_group (g:b_type) : b_type =
  match g with
  | Stripe -> Solid
  | Solid -> Stripe
  | _ -> failwith  "impossible"

(*resolve the ball sinking in a pocket
  Only called for first ball hit each turn*)
let resolve_sink (s:logic_state) (b:ball) : logic_state =
  if (get_type b = Cue)
    then {s with scratch = true;}
  else if (get_type b = Black) (*Sinking eight ball*)
    then
      let p = s.player in
        {s with
          player =
            if (p.balls_left = [])
              then {p with status = Won;}
            else {p with status = Lost;};
          other_player =
            if (p.balls_left = [])
              then {s.other_player with status = Lost;}
            else {s.other_player with status = Won;};
          game_over = true;}
  else if (s.break) (*if player did not yet decide ball type*)
  then
    {s with
    player =
      {s.player with
        group = get_type b; (*set group to be ball's group*)
        balls_left =
          remove_ball b
            (List.filter (fun x -> snd x = get_type b) s.player.balls_left);
        status = s.player.status;
      }; (*player just picked a side*)
    other_player =
      {s.other_player with
       group = opposite_group (get_type b); (*set group to be opposite ball's group*)
        balls_left =
          remove_ball b
            (List.filter (fun x -> snd x <> get_type b) s.other_player.balls_left); (*remove all balls of type not matching*)
      }; (*Opponent takes other side*)
    break = false; (*player just sunk valid ball*)
    continue = true; (*player just sunk valid ball*)
  }
  else if (get_type b = s.player.group)  (*sink own ball*)
  then {
    s with
      player =
          {s.player with balls_left = remove_ball b s.player.balls_left;}; (*1 less ball to sink*)
      continue = true;}(*sunk a good ball!*)
  else (*must have sunk ball of other person*)
    {s with other_player =
        {s.other_player with balls_left = remove_ball b s.other_player.balls_left;};}

(*returns the next state of the game
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let rec step_state (s:logic_state) (el:event list) : logic_state =
  match el with
  | [] | [None] -> {(next_turn s) with continue = false;}
  | h :: t -> match h with
              | None | Hit _ -> (*hitting other colored balls no longer matters*)
                      step_state s t
              | Sink b -> step_state (resolve_sink s b) t

(*takes the first step in calculating the next state
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let next_state (is:logic_state) (el:event list) : logic_state =
  match el with
  | [] | [None] -> (*scratch if no balls hit*)
      next_turn {is with scratch = true; continue = false;}
  | h :: t -> match h with
              | None -> step_state is t
              | Hit b -> step_state (resolve_hit is b) t;
              | Sink b -> if (get_type b = Cue) then
                  (*literally scratch is the only possibility*)
                  step_state (next_turn {is with scratch = true;}) t
                else failwith "Impossible to sink without hitting first"
