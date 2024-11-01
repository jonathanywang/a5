type t

type cell =
  | Empty
  | Occupied of Plant.t

val create : int -> int -> t
(** Create a new garden with given rows and columns. *)

val step : t -> unit
(** Advances the garden by one generation, updating plant growth and positions. *)

val add_plant : t -> int -> int -> unit
(** Add a plant to a specified position in the garden if it’s empty. *)

val print : t -> unit
(** Prints the current state of the garden in a text-based format. *)
