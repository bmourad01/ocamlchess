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

  (** The number of fullmoves to find a mate in. *)
  val mate : t -> int option

  (** The depth limit for the search, if any. *)
  val depth : t -> int option

  (** The time limit (in milliseconds) for the search, if any. *)
  val time : t -> int option

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

      By default, if no values are given for these parameters, the search is
      assumed to be infinite.

      The remaining parameters:

      - [active]: the active player, used for calculating the time limit
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
    active:Piece.color ->
    unit ->
    t
end

(** The search limits. *)
type limits = Limits.t

(** The transposition table. *)
module Tt : sig
  (** The bound for the score. *)
  type bound = Lower | Upper | Exact

  (** An entry in the table. *)
  module Entry : sig
    type t

    (** The depth that this entry was stored at. *)
    val depth : t -> int

    (** The score given for the position. *)
    val score : t -> int

    (** The best move for the position. *)
    val best : t -> Position.legal

    (** The bound for the score. *)
    val bound : t -> bound

    (** The position that this entry corresponds to. *)
    val position : t -> Position.t
  end

  (** An entry in the table. *)
  type entry = Entry.t

  (** The transposition table. *)
  type t

  (** Creates the table. *)
  val create : unit -> t

  (** Clears the table. *)
  val clear : t -> unit

  (** Returns the entry for a given position, if it exists *)
  val find : t -> Position.t -> entry option
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
  val score : t -> Uci.Send.Info.score

  (** The number of positions that were evaluated. *)
  val nodes : t -> int

  (** The depth that was searched to in order to obtain the result. *)
  val depth : t -> int

  (** The total time (in milliseconds) taken to complete the search.. *)
  val time : t -> int
end

(** The search result. *)
type result = Result.t

(** The callback function for each iteration of the search. *)
type iter = result -> unit

(** [go () ~root ~limits ~history ~tt ~iter ~stop] runs the game tree search
    and returns the search result. Raises [Invalid_argument] if there are no
    legal moves.

    - [root]: the position to start the search from.
    - [limits]: the search limits.
    - [history]: the history of how many times a position, indexed by 
      its Zobrist key, has occurred.
    - [tt]: the transposition table.

    An optional callback [iter] can be provided, which is invoked for each
    iteration of the search. By default, it will do nothing.

    An optional parameter [stop] may be provided to stop the search at an
    arbitrary point in time.
*)
val go :
  ?tt:Tt.t ->
  ?iter:iter ->
  ?stop:bool ref ->
  root:Position.t ->
  limits:limits ->
  history:int Core_kernel.Int64.Map.t ->
  unit ->
  result
