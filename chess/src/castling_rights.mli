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

  (** Returns [true] if it is on the king side. *)
  val is_kingside : t -> bool

  (** Returns [true] if it is on the queen side. *)
  val is_queenside : t -> bool
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

(** [compl x] returns the complement of castling rights for [x]. *)
val compl : t -> t

(** [minus x y] is equivalent to [inter x (compl y)]. *)
val minus : t -> t -> t

(** [mem x color side] tests whether [color] has castling rights on [side] in
    [x]. *)
val mem : t -> Piece.color -> side -> bool

(** [to_string x] returns the FEN string representation of [x]. *)
val to_string : t -> string

(** [of_string_exn s] parses [s] for a FEN string representaion of castling
    rights. Raises [Invalid_argument] if the string is invalid. Duplicate
    symbols are allowed. *)
val of_string_exn : string -> t

(** [of_string s] parses [s] for a FEN string representaion of castling
    rights. Returns [None] if the string is invalid. Duplicate symbols are
    allowed. *)
val of_string : string -> t option
