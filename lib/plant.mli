type t
(** The abstract type [t] represents a plant with a mutable age and a maximum
    age. *)

val get_age : t -> int
(** [get_age plant] is the current age of [plant]. *)

val get_max_age : t -> int
(** [get_max_age plant] is the maximum age of [plant]. *)

val create : unit -> t
(** [create ()] creates a new plant with an initial age of [0] and a maximum age
    of [9]. *)

val age : t -> unit
(** [age plant] increments the age of [plant] by one. *)

val is_dead : t -> bool
(** [is_dead plant] checks if [plant] is dead based on its age. *)

val print : t -> unit
(** [print plant] prints a visual representation of [plant]'s age using color. *)
