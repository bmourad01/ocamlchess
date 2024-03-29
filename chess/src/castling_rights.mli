(** This module implements a data structure for representing the castling
    rights of a position. *)

(** The number of bits required to represent castling rights. *)
val bits : int

(** The representation of castling rights as an unboxed integer. *)
type t = private int [@@deriving compare, equal, hash, sexp]

include Base.Comparable.S with type t := t

(** The side on which castling may occur. *)
type side = Kingside | Queenside [@@deriving compare, equal, sexp]

module Side : sig
  type t = side [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t

  (** The number of sides to castle on. *)
  val count : int

  (** Returns [true] if it is on the king side. *)
  val is_kingside : t -> bool

  (** Returns [true] if it is on the queen side. *)
  val is_queenside : t -> bool

  (** [of_int_exn i] returns the corresponding side of integer [i].
      Raises [Invalid_argument] if the value is not within range. *)
  val of_int_exn : int -> t

  (** [of_int_unsafe i] returns the corresponding side of integer [i].
      No santity checks are performed. *)
  val of_int_unsafe : int -> t

  (** [of_int i] returns the corresponding side of integer [i].
      Returns [None] if the value is not within range. *)
  val of_int : int -> t option

  (** [to_int side] returns the integer representation of [side]. *)
  val to_int : t -> int
end

(** Convert from integer representation. Raises [Invalid_argument] if the
    value is not within range. *)
val of_int_exn : int -> t

(** Convert from integer representation. Does not perform any sanity checks. *)
val of_int_unsafe : int -> t

(** Convert from integer representation. Returns [None] if the value is not
    within range. *)
val of_int : int -> t option

(** Returns the underlying integer representation. *)
val to_int : t -> int

(** No castling rights for either color. *)
val none : t

(** Castling rights for white, kingside only. *)
val white_kingside : t

(** Castling rights for white, queenside only. *)
val white_queenside : t

(** Full castling rights for white. *)
val white : t

(** Castling rights for black, kingside only. *)
val black_kingside : t

(** Castling rights for black, queenside only. *)
val black_queenside : t

(** Full castling rights for black. *)
val black : t

(** Castling rights for white and black, kingside only. *)
val kingside : t

(** Castling rights for white and black, queenside only. *)
val queenside : t

(** Full castling rights for both colors. *)
val all : t

(** [singleton c side] creates a singleton of castling rights for color [c]
    and side [side]. *)
val singleton : Piece.color -> side -> t

(** [inter x y] returns the intersection of castling rights for [x] and [y]. *)
val inter : t -> t -> t

(** [union x y] returns the union of castling rights for [x] and [y]. *)
val union : t -> t -> t

(** [compl cr] returns the complement of castling rights for [cr]. *)
val compl : t -> t

(** [minus x y] is equivalent to [inter x (compl y)]. *)
val minus : t -> t -> t

(** [mem x color side] tests whether [color] has castling rights on [side] in
    [x]. *)
val mem : t -> Piece.color -> side -> bool

(** [to_string cr] returns the FEN string representation of [cr]. *)
val to_string : t -> string

(** [pp ppf cr] pretty-prints [cr] to formatter [ppf]. *)
val pp : Format.formatter -> t -> unit

(** [of_string_exn s] parses [s] for a FEN string representaion of castling
    rights. Raises [Invalid_argument] if the string is invalid. Duplicate
    symbols are allowed. *)
val of_string_exn : string -> t

(** [of_string s] parses [s] for a FEN string representaion of castling
    rights. Returns [None] if the string is invalid. Duplicate symbols are
    allowed. *)
val of_string : string -> t option

module Syntax : sig
  (** [x & y] is equivalent to [inter x y]. *)
  val (&) : t -> t -> t

  (** [x + y] is equivalent to [union x y]. *)
  val (+) : t -> t -> t

  (** [x - y] is equivalent to [minus x y]. *)
  val (-) : t -> t -> t

  (** [~~x] is equivalent to [compl x]. *)
  val (~~) : t -> t

  (** [c --> s] is equivalent to [singleton c s]. *)
  val (-->) : Piece.color -> side -> t
end

include module type of Syntax
