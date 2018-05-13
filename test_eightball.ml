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
  ob = 0;
  collide = false;
  scratch= false;
  continue= false;
  game_over = false;
}

(*for pipelining*)
let n e i = next_state i e

let remove_ball (b : ball) (l: (int * b_type) list) : (int * b_type) list =
  List.filter (fun x -> fst x <> get_id b) l

let collision_tests = [
  "null" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [])
        {init_logic_state with player = init_logic_state.other_player; other_player = init_logic_state.player; scratch = true;}
    );
  "only hit" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit fifteen])
        {init_logic_state with player = init_logic_state.other_player; other_player = init_logic_state.player; break = false; scratch = true;}
    );
  "break" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit fifteen; Collide fifteen])
        {init_logic_state with player = init_logic_state.other_player; other_player = init_logic_state.player; break = false;}
    );
  "break_cue" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit fifteen; Collide cue])
        {init_logic_state with player = init_logic_state.other_player; other_player = init_logic_state.player; break = false;}
    );
  "pocket_eight" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit eight; Sink eight])
        {init_logic_state with player = {init_logic_state.player with status = Lost};
                               other_player = {init_logic_state.other_player with status = Won};
                               ob = 8; scratch = true; game_over = true;}
    );
  "sink_one" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit one; Sink one])
        {init_logic_state with player = {init_logic_state.player with balls_left = remove_ball one init_logic_state.player.balls_left};
                               other_player = {init_logic_state.other_player with balls_left = remove_ball one init_logic_state.other_player.balls_left};
                               break = false;}
    );

  "sink_all_solids" >:: (fun _ ->
      let stripes = [(9, Stripe); (10, Stripe); (11, Stripe);  (12, Stripe);  (13, Stripe);  (14, Stripe);  (15, Stripe);] in
      assert_equal
        (init_logic_state |> n [Hit one; Sink one; Sink two; Sink three; Sink four; Sink five; Sink six; Sink seven])
        {init_logic_state with player = {init_logic_state.player with balls_left = stripes};
                               other_player = {init_logic_state.other_player with balls_left = stripes};
                               break = false;}
    );

  "sink_all_balls" >:: (fun _ ->
      assert_equal
        (init_logic_state |> n [Hit one; Sink one; Sink two; Sink three; Sink four; Sink five; Sink six; Sink seven;
                               Sink nine; Sink ten; Sink eleven; Sink twelve; Sink thirteen; Sink fourteen; Sink fifteen])
        {init_logic_state with player = {init_logic_state.player with balls_left = []};
                               other_player = {init_logic_state.other_player with balls_left = []};
                               break = false;}
    );


]

let tests = List.flatten [collision_tests]

let suite = "8 Ball test suite" >::: tests

let _ = run_test_tt_main suite
