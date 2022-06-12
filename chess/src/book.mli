(** This module implements support for Polyglot opening books. *)

(** A database of games that can be used as an opening book for the
    computer player. *)
type t

(** [create filename] reads the book from the file [filename]. The
    file is expected to implement the Polyglot format. May raise if
    the file does not exist or the file is incorrectly formatted. *)
val create : string -> t

module Error : sig
  (** Errors that may be raised during lookup. *)
  type t =
    | Position_not_found of Position.t
    | Illegal_move of Move.t * Position.t
    | No_moves of Position.t

  (** Returns the error as a human-readable message. *)
  val to_string : t -> string
end

(** Errors that may be raised during lookup. *)
type error = Error.t

(** [lookup book pos] attempts to find an entry for [pos] in [book].

    Returns [Ok legal] if a valid entry is found, where [legal] is
    the legal move to be played in such a position, and [Error err]
    therwise.

    The response is chosen at random, modulo the weights assigned
    to certain moves (as described in the Polyglot format).
*)
val lookup : t -> Position.t -> (Position.legal, error) result
