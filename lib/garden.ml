type cell =
  | Empty
  | Occupied of Plant.t

type t = {
  grid : cell array array;
  rows : int;
  cols : int;
  mutable generation : int;
}

let get_gen garden = garden.generation
let get_cols garden = garden.cols
let get_rows garden = garden.rows
let get_grid garden = garden.grid

let create rows cols =
  let grid = Array.make_matrix rows cols Empty in
  { grid; rows; cols; generation = 0 }

let add_plant garden row col =
  if row >= 0 && row < garden.rows && col >= 0 && col < garden.cols then
    match garden.grid.(row).(col) with
    | Empty -> garden.grid.(row).(col) <- Occupied (Plant.create ())
    | _ -> ()

let step garden =
  garden.generation <- garden.generation + 1;
  let grid = garden.grid in
  let rows = garden.rows in
  let cols = garden.cols in

  (* Define directions locally within step *)
  let directions =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in

  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c cell ->
          match cell with
          | Empty ->
              (* Small chance of a new plant growing in empty cells *)
              if Random.int 100 < 3 then
                grid.(r).(c) <- Occupied (Plant.create ())
          | Occupied plant ->
              (* Check plant age and randomly decide to grow, stay the same, or
                 die *)
              if Plant.is_dead plant || Random.int 100 < 10 then (
                (* Plant dies, set to empty *)
                grid.(r).(c) <- Empty;

                (* Germination: chance for new plants in neighboring cells *)
                List.iter
                  (fun (dr, dc) ->
                    let nr = r + dr in
                    let nc = c + dc in
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                      match grid.(nr).(nc) with
                      | Empty ->
                          if Random.int 100 < 5 then
                            (* 5% chance to grow a new plant *)
                            grid.(nr).(nc) <- Occupied (Plant.create ())
                      | Occupied _ -> ())
                  directions)
              else (* Plant survives and ages *)
                Plant.age plant)
        row)
    grid

let print garden =
  Printf.printf "Generation: %d\n" garden.generation;
  Array.iter
    (fun row ->
      Array.iter
        (fun cell ->
          match cell with
          | Empty -> print_char '.'
          | Occupied plant -> Plant.print plant)
        row;
      print_newline ())
    garden.grid

(** (** Prints a visual representation of the garden. *) let print_garden plants
    = ANSITerminal.print_string [ ANSITerminal.on_black; ANSITerminal.white ]
    "Garden:\n"; List.iter (fun plant -> print plant; ANSITerminal.print_string
    [] " " (* Adds a space between plants *) ) plants; print_endline "" (* Move
    to the next line after printing all plants *) *)
