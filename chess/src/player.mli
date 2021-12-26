(** The interface used by a computer player. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player is expected to implement this interface.

    The [choose] method chooses which move to play based on the position.
    May raise [No_moves] if there are no legal moves available.

    The [limits] method returns the search limits, if any, that were used
    to construct the player.

    The [name] method returns the name of the player.
*)
type t = <
  choose : Position.legal_moves -> Position.legal_move;
  limits : limits option;
  name : string;
>

(** The instantiation signature. *)
type create = ?limits:limits option -> unit -> t

(** [best_moves moves ~eval] will take a list of moves, evaluate them with
    [eval], and then return a list of the highest scoring moves (with the
    same score). If [eval] returns [None] for a particular move, then it
    is discarded from the final solution. *)
val best_moves :
  Position.legal_move list ->
  eval:(Position.legal_move -> int option) ->
  Position.legal_move list
