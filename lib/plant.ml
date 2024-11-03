type t = {
  mutable age : int;
  max_age : int;
}

(* Accessor for age *)
let get_age plant = plant.age

(* Accessor for max_age *)
let get_max_age plant = plant.max_age
let create () = { age = 0; max_age = 9 }

(** Ages the plant by one step, possibly affecting its state. *)
let age plant = plant.age <- plant.age + 1

(** Checks if the plant is dead based on its age. *)
let is_dead plant = if plant.age > plant.max_age then true else false

(** Prints a visual representation of the plant’s age. *)
let print plant =
  if plant.age <= 3 then print_char '_'
  else if plant.age <= 6 then print_char 'o'
  else if plant.age <= 9 then print_char 'O'
  else print_char 'X'
