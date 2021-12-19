(** The interface used by a computer player. *)

(** Search limits. *)
type limits = {
  depth : int;
  nodes : int;
}

(** Raised when no legal moves are available for the player. *)
exception No_moves

(** The player interface. *)
type t = <move : Position.t -> Move.t * Position.t>

(** [player ~limits] will construct a player object with search limits [limits] *)
class virtual cls ?(limits = None) () = object
  (** The search limits, if they exist. *)
  val limits : limits option = limits

  (** [move pos] picks the move to play according to the player's evaluation
      of the position [pos]. This is a move/position pair, and it *must* be
      within the result returned by [Position.legal_moves pos]. If not, then
      undefined behavior may result. If the current position has no legal moves
      for the player, then [No_moves] is raised. *)
  method virtual move : Position.t -> Move.t * Position.t
end
