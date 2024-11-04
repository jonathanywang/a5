open A5.Garden
open A5.Plant
open Unix

(** [parse_args ()] parses command-line arguments to determine garden dimensions
    and frames per second (fps) for the simulation.
    @return
      A tuple (rows, cols, fps) where each is a positive integer, representing
      the number of rows, columns, and frames per second.
    @raise Failure if arguments are missing, non-integer, or not positive. *)
let parse_args () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "help" then (
    print_endline "Usage: dune exec bin/main.exe <rows> <cols> <fps>";
    print_endline "This program simulates plant growth in a garden.";
    print_endline
      "Characters indicate the plants’ age: '.' for young plants, 'o' for \
       middle-aged, 'O' for older plants, and 'X' for dead plants.";
    exit 0)
  else if Array.length Sys.argv <> 4 then (
    Printf.eprintf "Usage: %s <rows> <cols> <fps>\n" Sys.argv.(0);
    exit 1)
  else
    try
      let rows = int_of_string Sys.argv.(1) in
      let cols = int_of_string Sys.argv.(2) in
      let fps = int_of_string Sys.argv.(3) in
      if rows > 0 && cols > 0 && fps > 0 then (rows, cols, fps)
      else (
        Printf.eprintf
          "Error: Rows, columns, and FPS must all be positive integers.\n";
        exit 1)
    with Failure _ ->
      Printf.eprintf "Error: Rows, columns, and FPS must all be integers.\n";
      exit 1

(** [print_with_border garden] prints a bordered visual representation of
    [garden], displaying the current generation count and surrounding the grid
    with borders. Each cell in the garden grid is represented as:
    - '.' for empty cells,
    - the plant’s age character for occupied cells, based on age.

    @param garden The garden to display.
    @return unit. *)
let print_with_border garden =
  Printf.printf "Generation: %d\n" (A5.Garden.get_gen garden);
  let border = String.make (A5.Garden.get_cols garden + 2) '-' in
  print_endline border;
  Array.iter
    (fun row ->
      print_char '|';
      Array.iter
        (fun cell ->
          match cell with
          | Empty -> print_char '.'
          | Occupied plant -> A5.Plant.print plant)
        row;
      print_char '|';
      print_newline ())
    (A5.Garden.get_grid garden);
  print_endline border

(** [loop fps garden] runs the simulation loop, updating and rendering [garden]
    at the specified frames per second ([fps]). Each cycle advances the generation
    by calling [A5.Garden.step] and then renders the new state. The loop only stops
    upon interruption.
    @param fps The frame rate for simulation display, in frames per second.
    @param garden The garden to simulate.
    @return unit.
    @requires [fps] is a positive integer greater than zero.
    @examples
      - [loop 2 garden] starts a continuous simulation, refreshing the garden every
        0.5 seconds. *)
let rec loop fps garden =
  print_with_border garden;
  Unix.sleepf (1. /. float_of_int fps);
  A5.Garden.step garden;
  loop fps garden

(** Main program entry point.
    - Initializes garden dimensions and frames per second by calling
      [parse_args ()].
    - Sets up a signal handler to handle interrupt signals gracefully.
    - Starts the simulation loop by calling [loop fps garden].

    @return unit *)
let () =
  let rows, cols, fps = parse_args () in
  let garden = A5.Garden.create rows cols in
  Random.self_init ();
  Sys.set_signal Sys.sigint
    (Signal_handle
       (fun _ ->
         print_endline "\nExiting Simulation...";
         exit 0));
  loop fps garden
