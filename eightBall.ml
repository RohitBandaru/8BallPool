open Data
open Ball

(*get the next player to take a turn given the state and current player*)
let next_turn (s:logic_state) : logic_state =
  if (s.game_over) then s
  else if (s.scratch || not s.continue || not s.collide) (*switch players*)
  then {s with player = s.other_player;
               other_player = s.player;
               ob = 0;
               break = s.break && s.ob = 0;
               scratch = s.scratch || not s.collide;
               collide = false;
               continue = false;}
  else {s with ob = 0; break = s.break && s.ob = 0; collide = false; continue = false;}

  (*resolve the cue ball hitting another ball
    Only called for first ball hit each turn*)
  let resolve_hit (s:logic_state) (b:ball) : logic_state =
    if (*if ball hit is in legal balls to hit*)
      (List.exists (fun x -> fst x = get_id b) s.player.balls_left)
    then {s with ob = get_id b;}
    else {s with scratch = true; ob = get_id b;}

(*remove a ball from the list of balls*)
let remove_ball (b : ball) (l: (int * b_type) list) : (int * b_type) list =
  let ball_list = List.filter (fun x -> fst x <> get_id b) l in
  if ball_list = [] then [(8, Black)] else ball_list

(*get the opposite type of Stripes/Solids*)
let opposite_group (g:b_type) : b_type =
  match g with
  | Stripe -> Solid
  | Solid -> Stripe
  | _ -> failwith  "no opposite group"

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
            if (p.balls_left = [(8, Black)] && s.ob = get_id b)
              then {p with status = Won;}
            else {p with status = Lost;};
          other_player =
            if (p.balls_left = [(8, Black)] && s.ob = get_id b)
              then {s.other_player with status = Lost;}
            else {s.other_player with status = Won;};
          game_over = true;}
  else if (s.break) (*break*)
  then {
    s with
    player = {s.player with balls_left = remove_ball b s.player.balls_left;};
    other_player = {s.other_player with balls_left = remove_ball b s.other_player.balls_left;}; (*Both have 1 less ball to sink*)
    continue = true; collide = true;}
  else if (s.player.group = Cue) (*if player did not yet decide ball type*)
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
    continue = true; collide = true; (*player just sunk valid ball*)
  }
  else if (get_type b = s.player.group)  (*sink own ball*)
  then {
    s with
      player =
          {s.player with balls_left = remove_ball b s.player.balls_left;}; (*1 less ball to sink*)
      continue = true; collide = true;}(*sunk a good ball!*)
  else (*must have sunk ball of other person*)
    {s with other_player =
        {s.other_player with balls_left = remove_ball b s.other_player.balls_left;};}

let collider s b : logic_state =
  if (s.collide) (*if cue or target ball already hit a rail*)
  then s
  else if (get_id b = 0 || (*if cue ball or object ball*)
           get_id b = s.ob)
  then {s with collide = true;}
  else s

(*returns the next state of the game
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
 let rec step_state (s:logic_state) (el:event list) : logic_state =
  match el with
  | [] -> next_turn s
  | h :: t -> match h with
              | Collide b -> step_state (collider s b) t
              | Hit _ -> (*hitting other colored balls no longer matters*)
                      step_state s t
              | Sink b -> step_state (resolve_sink s b) t

(*takes the first step in calculating the next state
 *inputs: initial state and list of events this turn
 *returns: final state
 *)
let rec next_state (is:logic_state) (el:event list) : logic_state =
  let is = {is with scratch = false;} in (*assume scratch has been resolved by physics and game input*)
  match el with
  | [] -> (*scratch if no balls hit*)
      next_turn {is with scratch = true; continue = false;}
  | h :: t -> match h with
              | Collide b -> next_state (collider is b) t
              | Hit b -> step_state (resolve_hit is b) t;
              | Sink b -> if (get_type b = Cue) then
                  (*literally scratch is the only possibility*)
                  step_state (next_turn {is with scratch = true;}) t
                else failwith "Impossible to sink without hitting first"
