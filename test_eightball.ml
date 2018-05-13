open OUnit2
open Data
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

let init_logic_state : logic_state = {
  player = init_player;
  other_player = init_player;
  break= true;
  scratch= false;
  continue= false;
  game_over = false;
}

let collision_tests = [
  "none" >:: (fun _ ->
      assert_equal (next_state init_logic_state [None])
        {
        player = init_player;
        other_player = init_player;
        break= true;
        scratch= true;
        continue= false;
        game_over = false;
      }
    );

]

let tests = List.flatten [collision_tests]

let suite = "8 Ball test suite" >::: tests

let _ = run_test_tt_main suite
