(** The search limits. *)
module Limits : sig
  (** The search limits. *)
  type t

  (** Creates the search limits. Raises [Invalid_argument] on invalid
      inputs. *)
  val create :
    ?nodes:int option ->
    depth:int ->
    unit ->
    t

  (** The number of halfmoves that will be searched (e.g. the depth of
      the game tree). *)
  val depth : t -> int

  (** The limit on the number of positions that may be evaluated. *)
  val nodes : t -> int option
end

(** The search limits. *)
type limits = Limits.t

(** The search information. *)
type t

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

  (** The score that was given to the best move. *)
  val score : t -> int

  (** The number of positions that were evaluated. *)
  val evals : t -> int

  (** The depth that was searched to in order to obtain the result. *)
  val depth : t -> int
end

(** The search result. *)
type result = Result.t

(** Returns the search limits. *)
val limits : t -> limits

(** Returns the position to begin searching from. *)
val root : t -> Position.t

(** Returns the transposition table. *)
val tt : t -> Tt.t

(** Creates the search information.

    [history] is the history of positions, indexed by their
    Zobrist keys, coupled with the number of times they have occurred
    so far in the game.

    [tt] is the transposition table, used to cache search results.
*)
val create :
  ?tt:Tt.t ->
  limits:limits ->
  root:Position.t ->
  history:int Core_kernel.Int64.Map.t ->
  unit ->
  t

(** [go search ~clear] runs the game tree search and returns the search
    result. Raises [Invalid_argument] if there are no legal moves. *)
val go : t -> result
