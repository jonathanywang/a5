type t

val create : unit -> t
(** Creates a new plant. *)

val age : t -> unit
(** Ages the plant by one step, possibly affecting its state. *)

val is_dead : t -> bool
(** Checks if the plant is dead based on its age. *)

val print : t -> unit
(** Prints a visual representation of the plant’s age. *)
