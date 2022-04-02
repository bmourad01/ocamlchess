(** This module implements the data structures and algorithms used for game
    tree search. *)

(** The search limits. *)
module Limits : sig
  (** The search limits. *)
  type t

  (** The limit on the number of positions that may be evaluated. *)
  val nodes : t -> int option

  (** A search that never terminates unless interrupted. *)
  val infinite : t

  (** The depth limit for the search, if any. *)
  val depth : t -> int option

  (** The time limit (in milliseconds) for the search, if any. *)
  val time : t -> int option

  (** Returns [true] if the search is infinite. *)
  val is_infinite : t -> bool

  (** [of_depth depth ~nodes] will limit the depth of the search by [depth],
      and optionally the number of [nodes] that may be evaluated. Raises
      [Invalid_argument] on invalid inputs. *)
  val of_depth : ?nodes:int option -> int -> t

  (** [of_search_time t ~nodes] will limit the time allocated for the search
      by [t] milliseconds, and optionally the number of [nodes] that may be
      evaluated. Raises [Invalid_argument] on invalid inputs. *)
  val of_search_time : ?nodes:int option -> int -> t

  (** [of_game_time () ~wtime ~winc ~btime ~binc ~active ~nodes ~moves_to_go]
      will limit the time allocated for the search according to the following
      parameters (all times are in milliseconds):

      - [wtime]: the amount of time left on the clock for white.
      - [winc]: the time increment for white.
      - [btime]: the amount of time left on the clock for black.
      - [binc]: the time increment for black.
      - [active]: the active player.
      - [moves_to_go]: the (optional) number of moves left until the next time
        control.

      Additionally, the number of [nodes] to be evaluated may be limited.
      Raises [Invalid_argument] on invalid inputs.
  *)
  val of_game_time :
    ?nodes:int option ->
    ?moves_to_go:int option ->
    wtime:int ->
    winc:int ->
    btime:int ->
    binc:int ->
    active:Piece.color ->
    unit ->
    t
end

(** The search limits. *)
type limits = Limits.t

(** Returns [true] if the score is a mating win for the active player. *)
val is_mate : int -> bool

(** Returns [true] if the score is a mating loss for the active player. *)
val is_mated : int -> bool

(** The transposition table. *)
module Tt : sig
  (** The transposition table. *)
  type t

  (** Creates the table. *)
  val create : unit -> t

  (** Clears the table. *)
  val clear : t -> unit
end

(** The search result. *)
module Result : sig
  (** The search result. *)
  type t

  (** The best move to play. *)
  val best : t -> Position.legal

  (** The principal variation. *)
  val pv : t -> Position.legal list

  (** The score that was given to the best move. *)
  val score : t -> int

  (** The number of positions that were evaluated. *)
  val nodes : t -> int

  (** The depth that was searched to in order to obtain the result. *)
  val depth : t -> int

  (** The total time (in milliseconds) taken to complete the search.. *)
  val time : t -> int
end

(** The search result. *)
type result = Result.t

(** The callback function for each iteration of the search.

    A return value of [false] will stop the search prematurely. Otherwise,
    the search will continue normally.
*)
type iter = result -> bool

(** [go () ~root ~limits ~history ~tt ~iter] runs the game tree search and
    returns the search result. Raises [Invalid_argument] if there are no
    legal moves.

    - [root]: the position to start the search from.
    - [limits]: the search limits.
    - [history]: the history of how many times a position, indexed by 
      its Zobrist key, has occurred.
    - [tt]: the transposition table.

    An optional callback [iter] can be provided, which is invoked for each
    iteration of the search. By default, it will do nothing and return [true].
*)
val go :
  ?tt:Tt.t ->
  ?iter:iter ->
  root:Position.t ->
  limits:limits ->
  history:int Core_kernel.Int64.Map.t ->
  unit ->
  result
