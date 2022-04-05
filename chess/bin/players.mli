(** [register name player] will register [player] in the database. If a player
    with the same name already exists, then [Invalid_argument] is raised. *)
val register : string -> (unit -> Player.e) -> unit

(** [lookup name] will look up the player with the name [name]. If it does
    not exist, then [None] is returned. *)
val lookup : string -> Player.e option

(** [enumerate ()] will return a sequence of all currently registered players,
    sorted by name. *)
val enumerate : unit -> Player.e Base.Sequence.t
