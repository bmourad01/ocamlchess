(** This module implements an algorithm for statically evaluating Chess
    positions.

    It is essentially a linear combination of various features of a Chess
    position, each of which are weighted by hardcoded coefficients.
*)

(** Returns a score for a position, measured in centipawns.

    The score represents the relative advantage of the active player.
    Note that draws and checkmates are not explored here, but instead
    are considered when searching positions (see the [Search] module).

    Note that evaluations for tactically volatile positions (such as
    checks, captures, and promotions) are not guaranteed to be accurate.
*)
val go : Position.t -> int
