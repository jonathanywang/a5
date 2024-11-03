open OUnit2
open A5.Garden
open A5.Plant

let test_create expected_age expected_max =
  let plant = create () in
  assert_equal expected_age (A5.Plant.get_age plant);
  assert_equal expected_max (A5.Plant.get_max_age plant)

let test_age plant expected_age =
  A5.Plant.age plant;
  assert_equal expected_age (A5.Plant.get_age plant)

let test_is_dead plant expected_bool =
  assert_equal expected_bool (is_dead plant)

let plant_tests =
  "test suite for plant"
  >::: [
         ("testing create" >:: fun _ -> test_create 0 9);
         ( "testing age" >:: fun _ ->
           let plant = create () in
           test_age plant 1 );
         ( "testing is_dead" >:: fun _ ->
           let plant = create () in
           for _ = 1 to 10 do
             A5.Plant.age plant
           done;
           test_is_dead plant true );
       ]

let tests =
  "test suite" >::: [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]

let _ = run_test_tt_main plant_tests
