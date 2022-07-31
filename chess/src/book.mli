(** This module implements support for Polyglot opening books. *)

(** A database of games that can be used as an opening book for the
    computer player. *)
type t

(** [create filename] reads the book from the file [filename]. The
    file is expected to implement the Polyglot format. May raise if
    the file does not exist or the file is incorrectly formatted. *)
val create : string -> t

(** [filename book] returns the filename of the book. *)
val filename : t -> string

module Error : sig
  (** Errors that may be raised during lookup. *)
  type t =
    | Position_not_found of Position.t
    | Illegal_move of Move.t * Position.t
    | No_moves of Position.t

  (** Pretty-prints the error to a formatter. *)
  val pp : Format.formatter -> t -> unit

  (** Returns the error as a human-readable message. *)
  val to_string : t -> string
end

(** Errors that may be raised during lookup. *)
type error = Error.t

(** [lookup book pos ~skip_illegal ~random] attempts to find an entry for
    [pos] in [book].

    Returns [Ok child] if a valid entry is found, where [child] is the legal
    move to be played in such a position, and [Error err] otherwise.

    If [skip_illegal] is [false] (default), then an error is returned when an
    illegal move is found. Otherwise, such moves are ignored.

    If [random] is [true] (default), then the response is chosen at random,
    modulo the weights assigned to certain moves (as described in the Polyglot
    format). Otherwise, the highest-weighted move is picked.
*)
val lookup :
  ?skip_illegal:bool ->
  ?random:bool ->
  t ->
  Position.t ->
  (Position.child, error) result
