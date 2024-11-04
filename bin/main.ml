open A5.Garden
open A5.Plant
open Unix

(* Parses command-line arguments and returns rows, cols, and fps. *)
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

(* Visualizes the garden with a border and prints the current generation. *)
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

(* Runs the simulation loop at a given frames-per-second rate. *)
let rec loop fps garden =
  print_with_border garden;
  (* flush stdout; *)
  Unix.sleepf (1. /. float_of_int fps);
  A5.Garden.step garden;
  loop fps garden

(* Main program *)
let () =
  (* Parse command-line arguments for rows, cols, and fps *)
  let rows, cols, fps = parse_args () in
  (* Initialize the garden and random seed *)
  let garden = A5.Garden.create rows cols in
  Random.self_init ();
  (* Start simulation loop *)
  Sys.set_signal Sys.sigint
    (Signal_handle
       (fun _ ->
         print_endline "\nSimulation interrupted. Exiting...";
         exit 0));
  loop fps garden
