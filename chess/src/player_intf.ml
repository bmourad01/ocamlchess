(** The interface used by a computer player. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player is expected to implement this interface.

    - [choose pos moves] will choose, from [moves], which move to play based on
      the position [pos]. May raise [No_moves] if [moves] is empty. [moves]
      must be the list of moves derived from position [pos].

    - [limits] returns the search limits, if any, that were used to construct
      the player.

    - [name] returns the name of the player.
*)
type t = <
  choose : Position.t -> Position.legal list -> Position.legal;
  limits : limits option;
  name : string;
>

(** The instantiation signature. *)
type create = ?limits:limits option -> unit -> t
