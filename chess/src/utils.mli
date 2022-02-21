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

(** Branchless version of [Int.abs]. *)
val fast_abs : int -> int

(** Branchless version of [Int.min]. *)
val fast_min : int -> int -> int

(** Branchless version of [Int.max]. *)
val fast_max : int -> int -> int
