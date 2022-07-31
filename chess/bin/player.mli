(** This module exposes the interface used to implement computer players. *)

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** Raised when a move is chosen that is not withing the set
    of legal moves for a given position. *)
exception Invalid_move of Chess.Position.t * Chess.Position.child

(** The player whose internal state is of type ['a]. *)
type 'a t

(** Existential wrapper, for use in heterogeneous collections
    of players. *)
type e = T : 'a t -> e

(** The signature of the [choice] function. All players are
    expected to implement this interface.

    Given the current state of the player and a list of legal moves,
    return the preferred move along with an updated state.

    The list of moves is guaranteed to be non-empty, and each
    move in the list is derived from the same parent position.
*)
type 'a choice =
  'a -> Chess.Position.child list -> Chess.Position.child * 'a

(** Creates a new player.

    [limits] is the search limits of the player, if any.
    [choice] is the choice function for picking moves.
    [name] is the name of the player.
    [desc] is a description of the player.
    [state] is the initial/default state of the player.
*)
val create :
  choice:'a choice ->
  name:string ->
  desc:string ->
  state:'a ->
  'a t

(** [choose player pos] will choose the preferred legal move from the
    position [pos] according to the choice function of [player].
    Additionally, it will return an updated internal state of the player.

    If there are no legal moves for [pos], then [No_moves] is raised. 

    If the move chosen is not within the set of legal moves for [pos],
    then [Invalid_move] is raised.
*)
val choose : 'a t -> Chess.Position.t -> Chess.Position.child * 'a

(** Returns the name of the player. *)
val name : 'a t -> string

(** Returns a description of the player. *)
val desc : 'a t -> string

(** Returns the internal state of the player. *)
val state : 'a t -> 'a

(** [update player st] sets the internal state of [player] to [st],
    returning the updated player. *)
val update : 'a t -> 'a -> 'a t
