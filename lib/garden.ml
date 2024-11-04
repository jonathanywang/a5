open ANSITerminal

(** The type [cell] represents a grid cell that can either be [Empty] or
    [Occupied] by a plant. *)
type cell =
  | Empty
  | Occupied of Plant.t

type t = {
  grid : cell array array;
  rows : int;
  cols : int;
  mutable generation : int;
}
(** The abstract type [t] represents a garden with a grid, dimensions, and
    generation tracking. *)

(** [get_gen garden] is the current generation of [garden].
    @param garden the garden whose generation is accessed
    @return the generation number of [garden] *)
let get_gen garden = garden.generation

(** [get_cols garden] is the number of columns in [garden]'s grid.
    @param garden the garden whose column count is accessed
    @return the number of columns in [garden] *)
let get_cols garden = garden.cols

(** [get_rows garden] is the number of rows in [garden]'s grid.
    @param garden the garden whose row count is accessed
    @return the number of rows in [garden] *)
let get_rows garden = garden.rows

(** [get_grid garden] is the grid of cells in [garden].
    @param garden the garden whose grid is accessed
    @return the cell grid of [garden] *)
let get_grid garden = garden.grid

(** [create rows cols] creates a new garden with a grid of size [rows] by
    [cols]. The grid is initially filled with [Empty] cells, and the generation
    count is set to [0].
    @param rows the number of rows in the garden grid
    @param cols the number of columns in the garden grid
    @return a new garden of type [t] with specified dimensions *)
let create rows cols =
  let grid = Array.make_matrix rows cols Empty in
  { grid; rows; cols; generation = 0 }

(** [add_plant garden row col] adds a new plant to the cell at position ([row],
    [col]) in [garden]. If the cell is within bounds and empty, it will be
    occupied by a new plant.
    @param garden the garden in which to add the plant
    @param row the row index of the cell
    @param col the column index of the cell
    @return unit *)
let add_plant garden row col =
  if row >= 0 && row < garden.rows && col >= 0 && col < garden.cols then
    match garden.grid.(row).(col) with
    | Empty -> garden.grid.(row).(col) <- Occupied (Plant.create ())
    | _ -> ()

(** [step garden] advances the garden simulation by one generation, updating
    each cell:
    - Each occupied cell ages its plant, and may cause the plant to die.
    - If a plant dies, there is a chance for germination in neighboring cells.
    - Empty cells have a small chance of generating a new plant.

    @param garden the garden to update
    @return unit *)
let step garden =
  garden.generation <- garden.generation + 1;
  let grid = garden.grid in
  let rows = garden.rows in
  let cols = garden.cols in

  let directions =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in

  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c cell ->
          match cell with
          | Empty ->
              if Random.int 100 < 3 then
                grid.(r).(c) <- Occupied (Plant.create ())
          | Occupied plant ->
              if Plant.is_dead plant || Random.int 100 < 10 then (
                grid.(r).(c) <- Empty;
                List.iter
                  (fun (dr, dc) ->
                    let nr = r + dr in
                    let nc = c + dc in
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
                      match grid.(nr).(nc) with
                      | Empty ->
                          if Random.int 100 < 5 then
                            grid.(nr).(nc) <- Occupied (Plant.create ())
                      | Occupied _ -> ())
                  directions)
              else Plant.age plant)
        row)
    grid

(** [print garden] displays the garden's current state visually. Plants of
    different ages are displayed with varying colors:
    - Green "_" for young plants
    - Yellow "o" for moderately aged plants
    - Blue "O" for older plants
    - Red "X" for dead plants

    @param garden the garden to print
    @return unit *)
let print plants =
  List.iter
    (fun plant ->
      Plant.print plant;
      ANSITerminal.print_string [] " ")
    plants;
  print_endline ""
