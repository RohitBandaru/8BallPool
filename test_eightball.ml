open OUnit2
open Data
open Ball
open EightBall
open Printf

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

let init_other_player: player = {init_player with id = 1;}

let cue = create_ball "Cue" 0  Cue "" (0., 0.) (0., 0.) 0. 0.
let one = create_ball "1" 1  Solid "" (0., 0.) (0., 0.) 0. 0.
let two = create_ball "2" 2  Solid "" (0., 0.) (0., 0.) 0. 0.
let three = create_ball "3" 3  Solid "" (0., 0.) (0., 0.) 0. 0.
let four = create_ball "4" 4  Solid "" (0., 0.) (0., 0.) 0. 0.
let five = create_ball "5" 5  Solid "" (0., 0.) (0., 0.) 0. 0.
let six = create_ball "6" 6  Solid "" (0., 0.) (0., 0.) 0. 0.
let seven = create_ball "7" 7  Solid "" (0., 0.) (0., 0.) 0. 0.
let eight = create_ball "8" 8  Black "" (0., 0.) (0., 0.) 0. 0.
let nine = create_ball "9" 9  Stripe "" (0., 0.) (0., 0.) 0. 0.
let ten = create_ball "10" 10  Stripe "" (0., 0.) (0., 0.) 0. 0.
let eleven = create_ball "11" 11  Stripe "" (0., 0.) (0., 0.) 0. 0.
let twelve = create_ball "12" 12  Stripe "" (0., 0.) (0., 0.) 0. 0.
let thirteen = create_ball "13" 13  Stripe "" (0., 0.) (0., 0.) 0. 0.
let fourteen = create_ball "14" 14  Stripe "" (0., 0.) (0., 0.) 0. 0.
let fifteen = create_ball "15" 15  Stripe "" (0., 0.) (0., 0.) 0. 0.

let init_logic_state : logic_state = {
  player = init_player;
  other_player = init_other_player;
  break= true;
  scratch= false;
  continue= false;
  game_over = false;
}

let collision_tests = [
  "null" >:: (fun _ ->
      assert_equal {init_logic_state with scratch = true; player = init_other_player; other_player = init_player} (next_state init_logic_state [])
    );
  "none" >:: (fun _ ->
      assert_equal {init_logic_state with scratch = true; player = init_other_player; other_player = init_player} (next_state init_logic_state [None])
    );
  "break" >:: (fun _ ->
      assert_equal {init_logic_state with player = init_other_player; other_player = init_player} (next_state init_logic_state [Hit fifteen]) (*Since other_player = player*)
    );

]

let tests = List.flatten [collision_tests]

let suite = "8 Ball test suite" >::: tests

let _ = run_test_tt_main suite
