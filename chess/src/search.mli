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

(** Creates the search information. [transpositions] is the history of
    positions, indexed by their Zobrist keys, coupled with the number
    of times they have occurred so far in the game *)
val create :
  limits:limits ->
  root:Position.t ->
  transpositions:int Core_kernel.Int64.Map.t ->
  t

(** [go search] runs the game tree search and returns the search result.
    Raises [Invalid_argument] if there are no legal moves. *)
val go : t -> result
