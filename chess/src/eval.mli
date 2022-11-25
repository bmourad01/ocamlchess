(** This module implements an algorithm for statically evaluating Chess
    positions.

    It is essentially a linear combination of various features of a Chess
    position, each of which are weighted by hardcoded coefficients.
*)

(** This submodule exposes functionality for determining the current phase
    of the game. *)
module Phase : sig
  (** The maximum phase weight value  *)
  val maximum : int

  (** Returns the phase weight of the position. This is based on the total
      count of the non-pawn material on the board (excluding kings).

      The value returned is within the range [\[0, maximum\]].
  *)
  val weight : Position.t -> int

  (** Returns [true] if the non-pawn material on the board (excluding kings)
      is sufficiently low; thus, the position is in the endgame phase. *)
  val is_endgame : Position.t -> bool
end

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

(** Returns a score for a position, measured in centipawns.

    The score represents the relative advantage of the active player.
    Note that draws and checkmates are not explored here, but instead
    are considered when searching positions (see the [Search] module).

    Note that evaluations for tactically volatile positions (such as
    checks, captures, and promotions) are not guaranteed to be accurate.
*)
val go : Position.t -> int
