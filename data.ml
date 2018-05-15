open Ball

type game_type =
  |EightBall

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
  ob: int;
  collide: bool;
  scratch: bool;
  continue: bool;
  game_over: bool;
}

type state = logic_state * (ball list)

type event =
  | Collide of ball
  | Hit of ball
  | Sink of ball

type move =
  | None
  | Move of { player:player; velocity:(float*float) }
  | Place of (float * float)


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
  [
    (*create_ball "Cue" 0  Cue     "img/0.png" (-100.0, 100.0) (width /. 4.,  height /. 2.) (weight*.1.1) radius; (*Cue*)
    (*create_ball "Cue" 0  Cue     "img/0.png" (0.0, 100.0) (10.,  512. -. 26.) (weight*.1.1) radius; (*Cue*)*)
    (*create_ball "Cue" 0  Cue     "img/0.png" (100.0, 0.0) (900.,  15.) (weight*.1.1) radius; (*Cue*)*)
    (*create_ball "Cue" 0  Cue     "img/0.png" (0.0, -100.0) (1024.-.25.,  250.) (weight*.1.1) radius; (*Cue*)*)
    (*create_ball "Cue" 0  Cue     "img/0.png" (100.0, 0.0) (900.,  512. -. 26.) (weight*.1.1) radius; (*Cue*)*)
    *)
    (*create_ball "Cue" 0  Cue     "img/0.png" (100.0, 0.0) (900.,  26.) (weight*.1.1) radius; (*Cue*)

    create_ball "Cue" 1  Cue     "img/0.png" (100.0, -100.0) (width *.3. /. 4.,  height /. 2.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 2  Cue     "img/0.png" (0.0, -100.0) (24.,  height /. 4.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 3  Cue     "img/0.png" (-100.0, 0.0) (300.,  26.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 4  Cue     "img/0.png" (100.0, 0.0) (800.,  26.) (weight*.1.1) radius; (*Cue*)    
    create_ball "Cue" 5  Cue     "img/0.png" (0.0, 100.0) (1000.,  400.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 6  Cue     "img/0.png" (100.0, 0.0) (800.,  490.) (weight*.1.1) radius; (*Cue*)    
    create_ball "Cue" 7  Cue     "img/0.png" (0.0, 100.0) (24.,  400.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 8  Cue     "img/0.png" (-100.0, 0.0) (200.,  490.) (weight*.1.1) radius; (*Cue*)    
    *)
    create_ball "Cue" 0  Cue     "img/0.png" (0.0, -200.0) (506.,  512. -.100.) (weight*.1.1) radius; (*Cue*)
    (*create_ball "Cue" 1  Cue     "img/0.png" (0.0, 100.0) (534.,  512. -.100.) (weight*.1.1) radius; (*Cue*)
    create_ball "Cue" 2  Cue     "img/0.png" (0.0, -100.0) (478.,  100.) (weight*.1.1) radius; (*Cue*)
      create_ball "Cue" 3  Cue     "img/0.png" (0.0, -100.0) (534.,  100.) (weight*.1.1) radius; (*Cue*)*)

(*create_ball "Cue" 11  Cue     "img/0.png" (0.0, -100.0) (558.,  100.) (weight*.1.1) radius; (*Cue*)*)
    
(*    
    
    create_ball "Cue" 0  Cue     "img/0.png" (-450.0, 0.) (width *. 3. /. 4., height /. 2.) (weight*.1.1) radius; (*Cue*)
   create_ball "9"   9  Stripe  "img/9.png" init_vel (width /. 4. +. (2. *. sq32), height /. 2.) weight radius; (*Stripe*) 
   create_ball "7"   7  Solid   "img/7.png" init_vel (width /. 4. +. sq32, height /. 2. -. radius) weight radius; (*Solid*)
   create_ball "12" 12  Stripe  "img/12.png" init_vel (width /. 4. +. sq32, height /. 2. +. radius) weight radius; (*Stripe*)
   create_ball "15" 15  Stripe  "img/15.png" init_vel (width /. 4., height /. 2. -. (2. *. radius)) weight radius; (*Stripe*)
   create_ball "8"   8  Black   "img/8.png" init_vel (width /. 4., height /. 2.) weight radius; (*Eight*)
   create_ball "1"   1  Solid   "img/1.png" init_vel (width /. 4., height /. 2. +. (2. *. radius)) weight radius; (*Solid*)

   create_ball "6"   6  Solid   "img/6.png" init_vel (width /. 4. -. sq32, height /. 2. -. (3. *. radius)) weight radius; (*Solid*)
   
   create_ball "10" 10  Stripe  "img/10.png" init_vel (width /. 4. -. sq32, height /. 2. -. radius) weight radius; (*Stripe*)
   
   
create_ball "3"   3  Solid   "img/3.png" init_vel (width /. 4. -. sq32, height /. 2. +. radius) weight radius; (*Solid*)

   create_ball "14" 14  Stripe  "img/14.png" init_vel (width /. 4. -. sq32, height /. 2. +. (3. *. radius)) weight radius; (*Stripe*)
   
   create_ball "11" 11  Stripe  "img/11.png" init_vel (width /. 4. -. (2. *. sq32), height /. 2. -. (4. *. radius)) weight radius; (*Stripe*)
   create_ball "2"   2  Solid   "img/2.png" init_vel (width /. 4. -. (2. *. sq32), height /. 2. -. (2. *. radius)) weight radius; (*Solid*)
   create_ball "13" 13  Stripe  "img/13.png" init_vel (width /. 4. -. (2. *. sq32), height /. 2.) weight radius; (*Stripe*)
   create_ball "4"   4  Solid   "img/4.png" init_vel (width /. 4. -. (2. *. sq32), height /. 2. +. (2. *. radius)) weight radius; (*Solid*)
   create_ball "5"   5  Solid   "img/5.png" init_vel (width /. 4. -. (2. *. sq32), height /. 2. +. (4. *. radius)) weight radius; (*Solid*)
  *)  
  ]

let init_state (g:game_type) : state =
  match g with
  | EightBall ->
    ({
      player = init_player;
      other_player = {init_player with id = 1};
      break= true;
      ob = 0;
      collide = false;
      scratch= false;
      continue= false;
      game_over = false;
    }, eight_ball_init_ball_pos)

let get_logic (s:state) : logic_state =
  match s with
  | (logic, _) -> logic

let get_balls (s:state) : ball list = snd s

let ball_locations (s:state) : ball list = snd s

let next (s:state) : move =
  failwith "Unimplemented"
