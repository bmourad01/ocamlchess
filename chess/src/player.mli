(** The interface used to implement computer players. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player whose internal state is of type ['a]. *)
type 'a t

(** Existential wrapper, for use in heterogeneous collections
    of players. *)
type e = T : 'a t -> e

(** The signature of the [choice] function. All players are
    expected to implement this interface.

    Given the search limits, the current state of the player,
    and a list of legal moves, return the preferred move along
    with an updated state.

    The list of moves is guaranteed to be non-empty, and each
    move in the list is derived from the same parent position.
*)
type 'a choice =
  limits option -> 'a -> Position.legal list -> Position.legal * 'a

(** Creates a new player.

    [limits] is the search limits of the player, if any.
    [choice] is the choice function for picking moves.
    [name] is the name of the player.
    [desc] is a description of the player.
    [state] is the initial/default state of the player.
*)
val create :
  ?limits:limits option ->
  choice:'a choice ->
  name:string ->
  desc:string ->
  state:'a ->
  unit ->
  'a t

(** Returns the search limits of the player. *)
val limits : 'a t -> limits option

(** [choose player pos] will choose the preferred legal move from the
    position [pos] according to the choice function of [player].
    Additionally, it will return an updated internal state of the player.
    If there are no legal moves for [pos], then [No_moves] is raised. *)
val choose : 'a t -> Position.t -> Position.legal * 'a

(** Returns the name of the player. *)
val name : 'a t -> string

(** Returns a description of the player. *)
val desc : 'a t -> string

(** Returns the internal state of the player. *)
val state : 'a t -> 'a

(** [set_state player st] sets the internal state of [player] to [st],
    returning the new player. *)
val set_state : 'a t -> 'a -> 'a t

(** [register player] will register [player] in the database. If a player
    with the same name already exists, then [Invalid_argument] is raised. *)
val register : 'a t -> unit

(** [lookup name] will look up the player with the name [name]. If it does
    not exist, then [None] is returned. *)
val lookup : string -> e option

(** [enumerate ()] will return a list of all currently registered players,
    sorted by name. *)
val enumerate : unit -> e list
