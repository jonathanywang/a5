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
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c cell ->
          match cell with
          | Empty ->
              (* Small chance of a new plant growing *)
              if Random.int 100 < 5 then
                garden.grid.(r).(c) <- Occupied (Plant.create ())
          | Occupied plant ->
              (* Check plant age and randomly decide to grow, stay the same, or
                 die *)
              if Plant.is_dead plant || Random.int 100 < 10 then
                garden.grid.(r).(c) <- Empty (* Plant dies *)
              else Plant.age plant (* Plant ages *))
        row)
    garden.grid

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
