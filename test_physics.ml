open OUnit2
open Physics
open Ball
open Printf

let nw = Ball.create_ball "nw" "0" (-.1.0, 1.0) (0.5, -.0.5) 5.0 1.0
let sw = Ball.create_ball "sw" "0" (-.1.0, -.1.0) (0.0, 0.0) 5.0 1.0
let ne = Ball.create_ball "ne" "0" (1.0, 1.0) (-.0.5, -.0.5) 5.0 1.0
let se = Ball.create_ball "se" "0" (1.0, -.1.0) (-.0.5, 0.5) 5.0 1.0
let n  = Ball.create_ball "n" "0" (0.0, 1.0) (0.0, 0.0) 5.0 1.0
let s  = Ball.create_ball "s" "0" (0.0, -.1.0) (0.0, 0.0) 5.0 1.0
let e  = Ball.create_ball "e" "0" (1.0, 0.0) (0.0, 0.0) 5.0 1.0
let w  = Ball.create_ball "w" "0" (-.1.0, 0.0) (-.1.0, 0.0) 5.0 1.0
let c = Ball.create_ball "a" "0" (0.0, 0.0) (0.0, 0.0) 5.0 1.0

(*
let _ =  print_float (Physics.collide ne nw |> snd |> Ball.get_velocity |> fst);
  print_float (Physics.collide ne nw |> snd |> Ball.get_velocity |> snd);
  print_float (Physics.collide ne nw |> fst |> Ball.get_velocity |> fst);
  print_float (Physics.collide ne nw |> fst |> Ball.get_velocity |> snd)
  *)


                                                   
let cmp_float_tuple (a1, a2) (b1, b2)=
  cmp_float ~epsilon:1e-5 a1 b1 && cmp_float ~epsilon:1e-5 a2 b2

let _ = printf "%b" (cmp_float_tuple ((Physics.collide nw ne) |> snd |> Ball.get_velocity) (-1.0,1.0))

let collision_tests = [
  "orth1" >:: (fun _ ->
      assert_equal ((Physics.collide c w) |> fst |> Ball.get_velocity) (-.1.0, 0.0)
    );
  "orth2" >:: (fun _ ->
      assert_equal ((Physics.collide c w) |> snd |> Ball.get_velocity) (0.0, 0.0)
    );
  "orth3">:: (fun _ ->
      assert_equal ((Physics.collide nw ne) |> fst |> Ball.get_velocity) (1.0, 1.0)
    );
  "orth4">:: (fun _ ->
      assert_equal ~cmp:cmp_float_tuple (  (Physics.collide nw ne) |> snd |> Ball.get_velocity) (-1.0, 1.0)
    );
  "orth5">:: (fun _ ->
      assert_equal ~cmp:cmp_float_tuple (  (Physics.collide nw se) |> fst |> Ball.get_velocity) (1.0, -1.0)
    );
  "orth5">:: (fun _ ->
      assert_equal ~cmp:cmp_float_tuple (  (Physics.collide nw se) |> snd |> Ball.get_velocity) (-1.0, 1.0)
    );
  "collide1" >:: (fun _ ->
      assert_equal (Physics.is_overlap nw se) true
    );
  "collide2" >:: (fun _ ->
      assert_equal (Physics.is_overlap c w) true
    );
    
]

let tests = List.flatten [collision_tests]

let suite = "Physics test suite" >::: tests

let _ = run_test_tt_main suite

