(** The interface for AI players. *)

include module type of Player_intf

(** [register player] will register [player] in the database. If a player
    with the same name already exists, then [Invalid_argument] is raised. *)
val register : t -> unit

(** [lookup name] will look up the player with the name [name]. If it does
    not exist, then [None] is returned. *)
val lookup : string -> t option

(** [enumerate ()] will return a list of all currently registered players
    (in no particular order). *)
val enumerate : unit -> t list
