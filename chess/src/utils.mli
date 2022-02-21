(** Miscellaneous utilities. *)

(** Implements the xorshift64star pseudo-random number generator.
    This is a direct translation of the implementation used in Stockfish. *)
module Prng : sig
  (** The pseudo-random number generator.

      - [rand] will return a pseudo-random 64-bit integer.
  *)
  type t = < rand : int64 >

  (** [create seed] constructs the generator with [seed]. If [seed] is zero,
      then [Invalid_argument] is raised. *)
  val create : int64 -> t
end
