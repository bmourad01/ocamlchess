(** This module implements the algorithm for statically evaluating Chess
    positions. *)

(** This submodule exposes weights used in material evaluations, for both
    middle and endgame positions. *)
module Material : sig
  val pawn_mg : int
  val pawn_eg : int
  val knight_mg : int
  val knight_eg : int
  val bishop_mg : int
  val bishop_eg : int
  val rook_mg : int
  val rook_eg : int
  val queen_mg : int
  val queen_eg : int
end

(** Returns true if the position has approached endgame. *)
val is_endgame : Position.t -> bool

(** Returns a score for a position.

    The score represents the relative advantage of the active player.
    Note that draws and checkmates are not explored here, but instead
    are considered when searching positions (see the [Search] module).
*)
val go : Position.t -> int
