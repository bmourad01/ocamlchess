(** The interface used to implement computer players. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player is expected to implement this interface.

    - [choose moves] will choose, from [moves], which move to play. May
      raise [No_moves] if [moves] is empty. It is assumed that every
      element of [moves] have the same parent position.

    - [limits] returns the search limits, if any, that were used to construct
      the player.

    - [name] returns the name of the player.

    - [desc] returns a description of the player.
*)
type t = <
  choose : Position.legal list -> Position.legal;
  limits : limits option;
  name : string;
  desc : string;
>

(** [register player] will register [player] in the database. If a player
    with the same name already exists, then [Invalid_argument] is raised. *)
val register : t -> unit

(** [lookup name] will look up the player with the name [name]. If it does
    not exist, then [None] is returned. *)
val lookup : string -> t option

(** [enumerate ()] will return a list of all currently registered players,
    sorted by name. *)
val enumerate : unit -> t list
