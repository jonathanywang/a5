type t
(** The abstract type [t] represents a garden *)

(** The type [cell] represents a grid cell in the garden. *)
type cell =
  | Empty
  | Occupied of Plant.t

val get_gen : t -> int
(** [get_gen garden] returns the current generation number of the given garden. *)

val get_cols : t -> int
(** [get_cols garden] returns the number of columns in the grid of the given
    garden. *)

val get_rows : t -> int
(** [get_rows garden] returns the number of rows in the grid of the given
    garden. *)

val get_grid : t -> cell array array
(** [get_grid garden] returns the grid of cells in the given garden. *)

val create : int -> int -> t
(** [create rows cols] creates a new garden with a grid of dimensions [rows] by
    [cols]. *)

val step : t -> unit
(** [step] advances the garden by one generation, updating plant growth and
    positions. *)

val add_plant : t -> int -> int -> unit
(** [add_plant] adds a plant to a specified position in the garden if it’s
    empty. *)

val print : Plant.t list -> unit
(** [print] prints out the current state of the garden in a text-based format. *)
