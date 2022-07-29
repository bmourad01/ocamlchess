(** This module implements a data structure for representing Chess moves.

    Note that illegal moves can be instantiated with this datatype; the only
    information that is stored is the source square, destination square, and
    the (optional) promotion kind.

    For the legal moves, see the [Position.Legal] module and its corresponding
    datatype.
*)

(** The number of bits required to store the move information. *)
val bits : int

(** The representation of a move as an unboxed integer. The notation used is
    the same as the UCI protocol, with a source square, a destination square,
    and an optional piece promotion. *)
type t = private int [@@deriving compare, equal, hash, sexp]

include Base.Comparable.S with type t := t

module Promote : sig
  (** Piece promotion. *)
  type t = Knight | Bishop | Rook | Queen
  [@@deriving compare, equal, hash, sexp]

  include Base.Comparable.S with type t := t

  (** [of_piece_kind k] returns the associated promotion for piece kind [k].
      Raises [Invalid_argument] if [k] is not a valid promotion kind. *)
  val of_piece_kind : Piece.kind -> t

  (** [to_piece_kind p] returns the associated piece kind for promotion [p]. *)
  val to_piece_kind : t -> Piece.kind

  (** Returns the human-readable representation. *)
  val to_string_hum : t -> string
end

(** Piece promotion. *)
type promote = Promote.t

(** [create src dst ~promote] creates a move from square [src] to square
    [dst], with an optional promotion [promote]. *)
val create : ?promote:promote option -> Square.t -> Square.t -> t

(** [create_with_promote src dst promote] creates a move from square [src] to
    square [dst], with an explicit promotion [promote]. *)
val create_with_promote : Square.t -> Square.t -> promote -> t

(** [src m] returns the source square of move [m]. *)
val src : t -> Square.t

(** [dst m] returns the destination square of move [m]. *)
val dst : t -> Square.t

(** [promote m] returns the piece promotion of move [m], if it exists. *)
val promote : t -> promote option

(** [decomp m] returns a triple containing the source, destination and
    promotion of move [m], in that order. *)
val decomp : t -> Square.t * Square.t * promote option

(** [is_promote m] returns [true] if [m] is a promotion. *)
val is_promote : t -> bool

(** [with_src m src] sets the source square of [m] to [src]. *)
val with_src : t -> Square.t -> t

(** [with_dst m dst] sets the destination square of [m] to [dst]. *)
val with_dst : t -> Square.t -> t

(** [with_promote m promote] sets the promotion type of [m] to [promote]. *)
val with_promote : t -> promote -> t

(** [without_promote m] removes any promotion type from the move [m]. *)
val without_promote : t -> t

(** [to_string m] returns the UCI-compatible string notation of move [m]. *)
val to_string : t -> string

(** [pp ppf m] pretty-prints [m] to formatter [ppf]. *)
val pp : Format.formatter -> t -> unit

(** [of_string_exn s] parses [s] as a UCI-compatible string notation of a
    move, and raises [Invalid_argument] upon failure. *)
val of_string_exn : string -> t

(** [of_string s] parses [s] as a UCI-compatible string notation of a move,
    and returns [None] upon failure. *)
val of_string : string -> t option
