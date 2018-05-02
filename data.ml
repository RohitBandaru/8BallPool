open Ball

type game_type =
  |EightBallSolo
  |EightBallTwoP

type status =
  |Playing
  |Won
  |Lost

type player = {
  id: int;
  group: b_type;
  balls_left : (int * b_type) list;
  status : status;
}

type ball = t

type logic_state = {
  player: player;
  other_player: player;
  break: bool;
  scratch: bool;
  continue: bool;
  game_over: bool;
}

type state = logic_state * (ball list)

type event =
  | None
  | Hit of ball
  | Sink of ball

type move =
  | None
  | Move of { player:player; velocity:(float*float) }
  | Place of float * float


let init_player: player = {
  id = 0;
  group = Cue; (* init group does not matter as long as not Stripes or Solids*)
  balls_left =
          [(1, Solid);
           (2, Solid);
           (3, Solid);
           (4, Solid);
           (5, Solid);
           (6, Solid);
           (7, Solid);
           (9, Stripe);
           (10, Stripe);
           (11, Stripe);
           (12, Stripe);
           (13, Stripe);
           (14, Stripe);
           (15, Stripe);
          ];
  status = Playing;
}

let width = 1024. (*cm*)
let height = 512. (*cm*)
let weight = 0.156 (*kg*)
let radius = 11.4 (*cm*)
let init_vel = (0.,0.) (*cm*)

let sq32 = radius *. (sqrt 3.)

let eight_ball_init_ball_pos =
  (*TODO add stripe/solid/cue/black to constructor after updated in ball.ml*)
  [create_ball "Cue" 0  Cue "" init_vel (width *. 3. /. 4., height /. 2.) weight radius; (*Cue*)
   create_ball "9" 9  Stripe "" init_vel (width /. 4. -. (2. *. sq32), height /. 2.) weight radius; (*Stripe*)
   create_ball "7" 7  Solid "" init_vel (width /. 4. -. sq32, height /. 2. -. radius) weight radius; (*Solid*)
   create_ball "12" 12  Stripe "" init_vel (width /. 4. -. sq32, height /. 2. +. radius) weight radius; (*Stripe*)
   create_ball "15" 15  Stripe "" init_vel (width /. 4., height /. 2. -. (2. *. radius)) weight radius; (*Stripe*)
   create_ball "8" 8  Black "" init_vel (width /. 4., height /. 2.) weight radius; (*Eight*)
   create_ball "1" 1  Solid "" init_vel (width /. 4., height /. 2. +. (2. *. radius)) weight radius; (*Solid*)
   create_ball "6" 6  Solid "" init_vel (width /. 4. +. sq32, height /. 2. -. (3. *. radius)) weight radius; (*Solid*)
   create_ball "10" 10  Stripe "" init_vel (width /. 4. +. sq32, height /. 2. -. radius) weight radius; (*Stripe*)
   create_ball "3" 3  Solid "" init_vel (width /. 4. +. sq32, height /. 2. +. radius) weight radius; (*Solid*)
   create_ball "14" 14  Stripe "" init_vel (width /. 4. +. sq32, height /. 2. +. (3. *. radius)) weight radius; (*Stripe*)
   create_ball "11" 11  Stripe "" init_vel (width /. 4. +. (2. *. sq32), height /. 2. -. (4. *. radius)) weight radius; (*Stripe*)
   create_ball "2" 2  Solid "" init_vel (width /. 4. +. (2. *. sq32), height /. 2. -. (2. *. radius)) weight radius; (*Solid*)
   create_ball "13" 13  Stripe "" init_vel (width /. 4. +. (2. *. sq32), height /. 2.) weight radius; (*Stripe*)
   create_ball "4" 4  Solid "" init_vel (width /. 4. +. (2. *. sq32), height /. 2. +. (2. *. radius)) weight radius; (*Solid*)
   create_ball "5" 5  Solid "" init_vel (width /. 4. +. (2. *. sq32), height /. 2. +. (4. *. radius)) weight radius; (*Solid*)
  ]

let init_state (g:game_type) : state =
  match g with
  | EightBallSolo ->
    ({
      player = init_player;
      other_player = init_player;
      break= true;
      scratch= false;
      continue= false;
      game_over = false;
    }, eight_ball_init_ball_pos)
  | EightBallTwoP ->
    ({
      player = init_player;
      other_player = {init_player with id = 1};
      break= true;
      scratch= false;
      continue= false;
      game_over = false;
    }, eight_ball_init_ball_pos)

let ball_locations (s:state) : ball list = snd s

let move (s:state) : move =
  failwith "Unimplemented"
