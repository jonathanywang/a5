open ANSITerminal

type t = {
  mutable age : int;
  max_age : int;
}
(** The type [t] represents a plant with a mutable age and a maximum age.
    - [age]: the current age of the plant.
    - [max_age]: the age at which the plant is considered dead. *)

(** [get_age plant] is the current age of [plant].
    @param plant the plant whose age is accessed
    @return the age of [plant]
    @examples [get_age { age = 2; max_age = 9 }] is [2]. *)
let get_age plant = plant.age

(** [get_max_age plant] is the maximum age of [plant].
    @param plant the plant whose maximum age is accessed
    @return the maximum age of [plant]
    @examples [get_max_age { age = 2; max_age = 9 }] is [9]. *)
let get_max_age plant = plant.max_age

(** [create ()] creates a new plant with an initial age of [0] and a maximum age of [9].
    @return a new plant record with [age = 0] and [max_age = 9]
    @examples [create ()] is [{ age = 0; max_age = 9 }]. *)
let create () = { age = 0; max_age = 9 }

(** [age plant] increments the age of [plant] by one.
    @param plant the plant to age
    @return unit
    @examples
      [let plant = { age = 1; max_age = 9 } in
       age plant;
       get_age plant] is [2]. *)
let age plant = plant.age <- plant.age + 1

(** [is_dead plant] checks if [plant] is dead based on its age.
    @param plant the plant to check for life status
    @return [true] if [plant]'s age is greater than [plant.max_age], otherwise [false]
    @examples 
      [is_dead { age = 10; max_age = 9 }] is [true];
      [is_dead { age = 5; max_age = 9 }] is [false]. *)
let is_dead plant = if plant.age > plant.max_age then true else false

(** [print plant] prints a visual representation of [plant]'s age using color.
    - Green "_" if age is between [0] and [3].
    - Yellow "o" if age is between [4] and [6].
    - Blue "O" if age is between [7] and [9].
    - Red "X" if age is greater than [9].
    @param plant the plant to print
    @return unit
    @examples
      [print { age = 2; max_age = 9 }] prints a green "_";
      [print { age = 8; max_age = 9 }] prints a blue "O". *)
let print plant =
  if plant.age <= 3 then ANSITerminal.print_string [ ANSITerminal.green ] "_"
  else if plant.age <= 6 then
    ANSITerminal.print_string [ ANSITerminal.yellow ] "o"
  else if plant.age <= 9 then
    ANSITerminal.print_string [ ANSITerminal.blue ] "O"
  else ANSITerminal.print_string [ ANSITerminal.red ] "X"
