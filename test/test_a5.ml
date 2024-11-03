open OUnit2
open QCheck
open A5.Garden
open A5.Plant

let test_create_plant expected_age expected_max =
  let plant = A5.Plant.create () in
  assert_equal expected_age (A5.Plant.get_age plant);
  assert_equal expected_max (A5.Plant.get_max_age plant)

let test_age plant expected_age =
  A5.Plant.age plant;
  assert_equal expected_age (A5.Plant.get_age plant)

let test_is_dead plant expected_bool =
  assert_equal expected_bool (is_dead plant)

let test_create_garden expected_rows expected_cols expected_generation =
  let garden = A5.Garden.create expected_rows expected_cols in
  assert_equal expected_rows (A5.Garden.get_rows garden);
  assert_equal expected_cols (A5.Garden.get_cols garden);
  assert_equal expected_generation (A5.Garden.get_gen garden)

let test_add_plant garden row col expected_result =
  A5.Garden.add_plant garden row col;
  match (A5.Garden.get_grid garden).(row).(col) with
  | Occupied plant -> assert_equal expected_result (A5.Plant.get_age plant)
  | Empty -> assert_failure "Expected plant to be added but found Empty"

let test_step rows cols =
  let garden = A5.Garden.create rows cols in
  let initial_generation = A5.Garden.get_gen garden in

  (* Perform a step in the simulation *)
  A5.Garden.step garden;

  (* Check that the generation has incremented *)
  assert_equal (initial_generation + 1) (A5.Garden.get_gen garden)

(* QCheck test for randomly creating and aging plants *)
let qcheck_test_age_plant =
  QCheck.Test.make ~name:"test aging plant"
    (QCheck.pair (QCheck.int_range 0 10) (QCheck.int_range 1 10))
    (* Ensure initial_age is in [0, 10] range *)
      (fun (initial_age, increments) ->
      let plant = A5.Plant.create () in

      (* Simulate aging the plant to the initial age *)
      for _ = 1 to initial_age do
        A5.Plant.age plant
      done;

      (* Age the plant further by the specified increments *)
      for _ = 1 to increments do
        A5.Plant.age plant
      done;

      (* Verify that age did not decrease and stays within bounds *)
      let final_age = A5.Plant.get_age plant in
      final_age
      = initial_age + increments (* Ensure final_age is exactly as expected *))

(* QCheck test for adding plants to the garden with random positions *)
let qcheck_test_add_plant =
  QCheck.Test.make ~name:"test add plant to garden"
    (QCheck.pair (QCheck.int_range 0 4) (QCheck.int_range 0 6))
    (* Adjust range to match garden dimensions *)
      (fun (row, col) ->
      let garden = A5.Garden.create 5 7 in
      A5.Garden.add_plant garden row col;
      (* Attempt to add a plant *)
      match (A5.Garden.get_grid garden).(row).(col) with
      | Occupied plant ->
          let expected_age = A5.Plant.get_age plant in
          expected_age
          >= 0 (* Check that a plant was added and has a non-negative age *)
      | Empty -> false (* If the cell is still empty, the test should fail *))

let plant_tests =
  "test suite for plant"
  >::: [
         ("testing create" >:: fun _ -> test_create_plant 0 9);
         ( "testing age" >:: fun _ ->
           let plant = A5.Plant.create () in
           test_age plant 1 );
         ( "testing is_dead" >:: fun _ ->
           let plant = A5.Plant.create () in
           for _ = 1 to 10 do
             A5.Plant.age plant
           done;
           test_is_dead plant true );
         (* Add QCheck tests for plants *)
         QCheck_runner.to_ounit2_test qcheck_test_age_plant;
         QCheck_runner.to_ounit2_test qcheck_test_add_plant;
       ]

let garden_tests =
  "test suite for garden"
  >::: [
         ("testing create" >:: fun _ -> test_create_garden 5 7 0);
         ( "testing add_plant" >:: fun _ ->
           let garden = A5.Garden.create 5 7 in
           test_add_plant garden 0 0 0;
           test_add_plant garden 1 1 0 );
         ("testing step" >:: fun _ -> test_step 3 4);
       ]

let tests = "combined test suite" >::: [ plant_tests; garden_tests ]
let _ = run_test_tt_main tests
