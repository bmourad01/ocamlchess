(** The interface used by a computer player. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The function that chooses which move to play, based on the position.
    May raise [No_moves] if there are no legal moves available. *)
type choose = Position.legal_moves -> Position.legal_move

(** The player. *)
type t

(** [choose player pos] returns the choice function. *)
val choose : t -> choose

(** [limits player] returns the search limits on the player, if any. *)
val limits : t -> limits option

(** The instantiation signature. *)
type create = ?limits:limits option -> unit -> t

(** The instantiation function. *)
val create : choose:choose -> create

(** [best_moves moves ~eval] will take a list of moves, evaluate them with
    [eval], and then return a list of the highest scoring moves (with the
    same score). If [eval] returns [None] for a particular move, then it
    is discarded from the final solution. *)
val best_moves :
  Position.legal_move list ->
  eval:(Position.legal_move -> int option) ->
  Position.legal_move list
