type cell =
  | Empty
  | Occupied of Plant.t

type t = {
  grid : cell array array;
  rows : int;
  cols : int;
  mutable generation : int;
}

let create rows cols =
  let grid = Array.make_matrix rows cols Empty in
  { grid; rows; cols; generation = 0 }

let add_plant garden row col =
  if row >= 0 && row < garden.rows && col >= 0 && col < garden.cols then
    match garden.grid.(row).(col) with
    | Empty -> garden.grid.(row).(col) <- Occupied (Plant.create ())
    | _ -> ()

let step garden = garden.generation <- garden.generation + 1
(* Insert logic for plant aging, death, and propagation *)

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
