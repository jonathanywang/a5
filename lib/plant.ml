type t = {
  mutable age : int;
  max_age : int;
}

(** Creates a new plant. *)
let create () = { age = 0; max_age = 9 }

(** Ages the plant by one step, possibly affecting its state. *)
let age plant = plant.age <- plant.age + 1

(** Checks if the plant is dead based on its age. *)
let is_dead plant = if plant.age > plant.max_age then true else false

(** Prints a visual representation of the plant’s age. *)
let print plant =
  if plant.age <= 3 then print_char '.'
  else if plant.age <= 6 then print_char 'o'
  else if plant.age <= 9 then print_char 'O'
  else print_char 'X'
