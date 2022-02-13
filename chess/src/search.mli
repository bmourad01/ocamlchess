(** The search limits. *)
module Limits : sig
  (** The search limits.
    
      [depth] is the number of halfmoves that will be
      searched (e.g. the depth of the game tree).

      [nodes] is the optional limit on the number of
      positions that may be evaluated.
  *)
  type t = {
    depth : int;
    nodes : int option;
  }
end

(** The search limits. *)
type limits = Limits.t

(** The search information. *)
type t

(** Returns the search limits. *)
val limits : t -> limits

(** Returns the position to begin searching from. *)
val root : t -> Position.t

(** Creates the search information. *)
val create :
  limits:limits ->
  root:Position.t ->
  transpositions:int Core_kernel.Int64.Map.t ->
  t

(** [go search] runs the game tree search and returns the most
    favorable move along with its score. *)
val go : t -> Position.legal * int
