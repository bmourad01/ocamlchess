(** This module implements the data structures and algorithms used for game
    tree search. *)

(** The search limits. *)
module Limits : sig
  (** The search limits, according to the UCI protocol. *)
  type t

  (** Returns [true] if the search is infinite. *)
  val infinite : t -> bool

  (** The limit on the number of positions that may be evaluated. *)
  val nodes : t -> int option

  (** The number of fullmoves [n] to find a mate in.

      Positive values of [n] indicate a winning checkmate for the active
      player. Likewise, negative values indicate a losing checkmate.

      If [n] is zero and the position to search is not checkmate, then the
      search is aborted.
  *)
  val mate : t -> int option

  (** The depth limit for the search, if any. *)
  val depth : t -> int option

  (** The exact amount of time (in milliseconds) to search. *)
  val movetime : t -> int option

  (** The maximum amount of time (in milliseconds) that the active player
      has available to move. *)
  val max_time : t -> int option

  (** The moves that will be exclusively searched (if any). *)
  val moves : t -> Move.t list

  (** The default depth to search to. *)
  val default_depth : int

  (** Creates the search limits, raising [Invalid_argument] if the parameters
      are ill-formed.

      The following parameters control the limits:

      - [nodes]: search [n] nodes only
      - [mate]: search for a mate in [n] moves
      - [depth]: search [n] plies only
      - [movetime]: search exactly [n] milliseconds
      - [movesgoto]: there are [n] moves to the next time control
      - [wtime]: white has [n] milliseconds left on the clock
      - [winc]: white gets [n] millisecond increments per move
      - [btime]: black has [n] milliseconds left on the clock
      - [binc]: black gets [n] millisecond increments per move
      - [infinite]: search until the [stop] command.
      - [moves]: If not empty, this is the list of moves that will be
        exclusively searched.

      By default, if no values are given for these parameters, the search will
      use the [depth] value of [default_depth]. If all parameters are left
      explicitly unspecified (e.g. [infinite] is [false], and all others are
      [None]), then [Invalid_argument] is raised.

      The remaining parameters:

      - [active]: the active player, used for calculating the time limit
      - [stop]: a promise to stop the search at an arbitrary point in time
  *)
  val create :
    ?nodes:int option ->
    ?mate:int option ->
    ?depth:int option ->
    ?movetime:int option ->
    ?movestogo:int option ->
    ?wtime:int option ->
    ?winc:int option ->
    ?btime:int option ->
    ?binc:int option ->
    ?infinite:bool ->
    ?moves:Move.t list ->
    active:Piece.color ->
    stop:unit Bap_future.Std.future ->
    unit ->
    t
end

(** The search limits. *)
type limits = Limits.t

(** The transposition table. *)
module Tt : sig
  (** An entry in the table. *)
  module Entry : sig
    type t

    (** The depth that this entry was stored at. *)
    val depth : t -> int

    (** The score given for the position. *)
    val score : t -> int

    (** The static evaluation of the position. *)
    val eval : t -> int option

    (** The best move for the position. *)
    val best : t -> Position.child option

    (** The bound for the position's score. *)
    val bound : t -> Uci.Send.Info.bound
  end

  type entry = Entry.t
  type t

  (** [create ?len ()] creates a fresh table, where [len] is the maximum
      number of entries allowed. *)
  val create : ?len:int -> unit -> t

  (** Clears all entries in the table. *)
  val clear : t -> unit

  (** Finds the entry associated with a given position. Returns [None] if
      the entry does not exist in the table. *)
  val find : t -> Position.t -> entry option
end

type tt = Tt.t

(** The search result. *)
module Result : sig
  (** The search result. *)
  type t

  (** The principal variation (PV). This is the engine's preferred line of
      play for both sides. Thus, the first element of the PV is the engine's
      preferred move for the position that was searched.

      If this is not a mating sequence, then it is guaranteed to have a length
      that is at most the depth that was searched.
  *)
  val pv : t -> Position.child list

  (** The best move to play, if any. This is the first element of the PV. *)
  val best : t -> Position.child option

  (** The best move to play. Raises if the PV is empty. *)
  val best_exn : t -> Position.child

  (** The score that was given to the best move. *)
  val score : t -> Uci.Send.Info.score

  (** The number of nodes (positions) that were searched. *)
  val nodes : t -> int

  (** The depth limit of the search. *)
  val depth : t -> int

  (** The selective search depth in plies. This is the actual maximum depth
      that was searched, which may be less or greater than the actual depth
      limit. This may happen if the engine decides to extend the search for
      more interesting lines, or reduce the search for lines which are
      unlikely to improve the position. *)
  val seldepth : t -> int

  (** The total time (in milliseconds) taken to complete the search. *)
  val time : t -> int
end

(** The search result. *)
type result = Result.t

(** [go () ~root ~limits ~history ~tt ~iter ~stop] runs the game tree search
    and returns the search result.

    - [root]: the position to start the search from.
    - [limits]: the search limits.
    - [history]: the history of how many times a position, indexed by 
      its Zobrist key, has occurred. A local copy of this table is made
      for the duration of the search, so there is no risk of mutating
      the existing entries.
    - [tt]: the transposition table.

    An optional callback [iter] can be provided, which is invoked for each
    iteration of the search. By default, it will do nothing.

    An optional future [ponder] can be provided. If it is [None] (default),
    then the search will run normally. Otherwise, it will run in ponder mode
    until this future is decided.
*)
val go :
  ?iter:(result -> unit) ->
  ?ponder:unit Bap_future.Std.future option ->
  root:Position.t ->
  limits:limits ->
  history:(Zobrist.key, int) Base.Hashtbl.t ->
  tt:tt ->
  unit ->
  result
