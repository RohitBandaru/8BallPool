open OUnit2
open Physics
open Ball

let nw = Ball.create_ball "nw" "0" (-.1.0, 1.0) (0.0, 0.0) 5.0 1.0
let sw = Ball.create_ball "nw" "0" (-.1.0, -.1.0) (0.0, 0.0) 5.0 1.0
let ne = Ball.create_ball "nw" "0" (1.0, 1.0) (0.0, 0.0) 5.0 1.0
let se = Ball.create_ball "nw" "0" (1.0, -.1.0) (0.0, 0.0) 5.0 1.0
let n  = Ball.create_ball "nw" "0" (0.0, 1.0) (0.0, 0.0) 5.0 1.0
let s  = Ball.create_ball "nw" "0" (0.0, -.1.0) (0.0, 0.0) 5.0 1.0
let e  = Ball.create_ball "nw" "0" (1.0, 0.0) (0.0, 0.0) 5.0 1.0
let w  = Ball.create_ball "nw" "0" (-.1.0, 0.0) (0.0, 0.0) 5.0 1.0

let collision_tests = [
  "orth1" >:: (fun _ ->
      assert_equal 5 5
    )
]

let tests = List.flatten [collision_tests]

let suite = "Physics test suite" >::: tests

let _ = run_test_tt_main suite
