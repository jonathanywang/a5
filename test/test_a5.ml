(** This file contains unit tests and property-based tests for the [Plant] and
    [Garden] modules within the A5 project. Tests are structured using OUnit2
    for standard unit tests and QCheck for randomized property-based tests. *)

open OUnit2
open QCheck
open A5.Garden
open A5.Plant

(** [test_create_plant expected_age expected_max] checks that a newly created
    plant has the specified initial age and maximum age. *)
let test_create_plant expected_age expected_max =
  let plant = A5.Plant.create () in
  assert_equal expected_age (A5.Plant.get_age plant);
  assert_equal expected_max (A5.Plant.get_max_age plant)

(** [test_age plant expected_age] verifies that aging a plant updates its age to
    the expected value. *)
let test_age plant expected_age =
  A5.Plant.age plant;
  assert_equal expected_age (A5.Plant.get_age plant)

(** [test_is_dead plant expected_bool] checks that [is_dead] correctly
    identifies if a plant is dead based on the expected boolean value. *)
let test_is_dead plant expected_bool =
  assert_equal expected_bool (is_dead plant)

(** [test_create_garden expected_rows expected_cols expected_generation]
    verifies that a new garden is created with the specified dimensions and
    initial generation count. *)
let test_create_garden expected_rows expected_cols expected_generation =
  let garden = A5.Garden.create expected_rows expected_cols in
  assert_equal expected_rows (A5.Garden.get_rows garden);
  assert_equal expected_cols (A5.Garden.get_cols garden);
  assert_equal expected_generation (A5.Garden.get_gen garden)

(** [test_add_plant garden row col expected_result] checks that adding a plant
    at the specified position in the garden results in an [Occupied] cell with a
    plant of the expected initial age. *)
let test_add_plant garden row col expected_result =
  A5.Garden.add_plant garden row col;
  match (A5.Garden.get_grid garden).(row).(col) with
  | Occupied plant -> assert_equal expected_result (A5.Plant.get_age plant)
  | Empty -> assert_failure "Expected plant to be added but found Empty"

(** [test_step rows cols] verifies that advancing the garden simulation by one
    generation increments the generation counter as expected. *)
let test_step rows cols =
  let garden = A5.Garden.create rows cols in
  let initial_generation = A5.Garden.get_gen garden in
  A5.Garden.step garden;
  assert_equal (initial_generation + 1) (A5.Garden.get_gen garden)

(** [test_step_germination rows cols] checks that an empty garden has a chance
    of generating new plants in empty cells when [step] is called. *)
let test_step_germination rows cols =
  let garden = A5.Garden.create rows cols in
  (* Advance the simulation multiple times to increase the probability of
     germination *)
  for _ = 1 to 100 do
    A5.Garden.step garden
  done;
  (* Check if at least one cell is occupied after several steps *)
  let has_plant =
    Array.exists
      (Array.exists (function
        | Occupied _ -> true
        | Empty -> false))
      (A5.Garden.get_grid garden)
  in
  assert_bool "Expected at least one cell to be occupied by a new plant"
    has_plant

(** [test_step_death_and_spread rows cols] places a plant in the garden, ages it
    until death, and checks if neighboring cells have a chance of germination. *)
let test_step_death_and_spread rows cols =
  let garden = A5.Garden.create rows cols in
  A5.Garden.add_plant garden 2 2;
  let rec age_plant_until_spread () =
    match (A5.Garden.get_grid garden).(2).(2) with
    | Occupied plant when not (A5.Plant.is_dead plant) ->
        A5.Garden.step garden;
        age_plant_until_spread ()
    | _ ->
        let directions =
          [
            (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1);
          ]
        in
        let has_spread =
          List.exists
            (fun (dr, dc) ->
              let nr = 2 + dr in
              let nc = 2 + dc in
              nr >= 0 && nr < rows && nc >= 0 && nc < cols
              &&
              match (A5.Garden.get_grid garden).(nr).(nc) with
              | Occupied _ -> true
              | Empty -> false)
            directions
        in
        assert_bool
          "Expected at least one neighboring cell to be occupied after plant \
           death"
          has_spread
  in
  age_plant_until_spread ()

(** [test_step_edge_cases] verifies that the plant spreading and germination
    logic does not allow plants to be placed out of bounds, particularly near
    the edges and corners of the grid. *)
let test_step_edge_cases () =
  let garden = A5.Garden.create 3 3 in
  A5.Garden.add_plant garden 0 0;
  A5.Garden.step garden;
  let grid = A5.Garden.get_grid garden in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c cell ->
          match cell with
          | Occupied _ ->
              if r < 0 || r >= 3 || c < 0 || c >= 3 then
                assert_failure "Spread should not go out of bounds"
          | Empty -> ())
        row)
    grid

(** [qcheck_test_age_plant] is a QCheck test that verifies plant aging behavior
    with randomized initial ages and increments. It ensures that a plant’s age
    increases as expected and does not decrease. *)
let qcheck_test_age_plant =
  QCheck.Test.make ~name:"test aging plant"
    (QCheck.pair (QCheck.int_range 0 10) (QCheck.int_range 1 10))
    (fun (initial_age, increments) ->
      let plant = A5.Plant.create () in
      for _ = 1 to initial_age do
        A5.Plant.age plant
      done;
      for _ = 1 to increments do
        A5.Plant.age plant
      done;
      let final_age = A5.Plant.get_age plant in
      final_age = initial_age + increments)

(** [qcheck_test_add_plant] is a QCheck test that verifies adding a plant at
    random positions in a 5x7 garden grid. It checks that the cell at the
    specified position becomes occupied by a plant with a non-negative age. *)
let qcheck_test_add_plant =
  QCheck.Test.make ~name:"test add plant to garden"
    (QCheck.pair (QCheck.int_range 0 4) (QCheck.int_range 0 6))
    (fun (row, col) ->
      let garden = A5.Garden.create 5 7 in
      A5.Garden.add_plant garden row col;
      match (A5.Garden.get_grid garden).(row).(col) with
      | Occupied plant ->
          let expected_age = A5.Plant.get_age plant in
          expected_age >= 0
      | Empty -> false)

(** [plant_tests] is a test suite for [Plant] functions, containing both OUnit2
    unit tests and QCheck property-based tests. *)
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
         QCheck_runner.to_ounit2_test qcheck_test_age_plant;
         QCheck_runner.to_ounit2_test qcheck_test_add_plant;
       ]

(** [garden_tests] is a test suite for [Garden] functions, including unit tests
    to verify garden creation, plant addition, and simulation stepping. *)
let garden_tests =
  "test suite for garden"
  >::: [
         ("testing create" >:: fun _ -> test_create_garden 5 7 0);
         ( "testing add_plant" >:: fun _ ->
           let garden = A5.Garden.create 5 7 in
           test_add_plant garden 2 3 0 );
         ("testing step" >:: fun _ -> test_step 5 7);
         ("testing step germination" >:: fun _ -> test_step_germination 5 7);
         ( "testing step death and spread" >:: fun _ ->
           test_step_death_and_spread 5 7 );
         ("testing step edge cases" >:: fun _ -> test_step_edge_cases ());
       ]

(** [tests] is the main test suite that combines both [Plant] and [Garden] test
    suites. *)
let tests = "test suite for A5 project" >::: [ plant_tests; garden_tests ]

let _ = run_test_tt_main tests
