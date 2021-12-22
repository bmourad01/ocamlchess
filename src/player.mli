(** The interface used by a computer player. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player record:

    - [choose] is the function that chooses which move to play,
      based on the position. May raise [No_moves] if there are no
      legal moves available.
    - [limits] are the search limits on the player, if any. *)
type t = {
  choose : Position.t -> Position.legal_move;
  limits : limits option;
}

(** The instantiation signature. *)
type create = ?limits:limits option -> unit -> t

(** [equal_eval moves ~eval] will take a list of moves, evaluate them with
    [eval], and then return a list of the highest scoring moves (with the
    same score). If [eval] returns [None] for a particular move, then it
    is discarded from the final solution. *)
val equal_eval :
  Position.legal_move list ->
  eval:(Position.legal_move -> int option) ->
  Position.legal_move list
