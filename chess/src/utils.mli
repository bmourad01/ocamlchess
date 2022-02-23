(** Miscellaneous utilities. *)

(** Implements the xorshift64star pseudo-random number generator.
    This is a direct translation of the implementation used in Stockfish. *)
module Prng : sig
  (** The pseudo-random number generator. *)
  type t = unit -> int64

  (** [create seed] constructs the generator with [seed]. If [seed] is zero,
      then [Invalid_argument] is raised. *)
  val create : int64 -> t
end
