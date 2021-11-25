open Base

(** Number of bits needed to store the color. *)

val color_bits : int

(** Number of bits needed to store the kind. *)

val kind_bits : int

(** Number of bits to store the entire piece. *)

val bits : int

(** The color of a piece. *)
type color = White | Black [@@deriving compare, equal, hash, sexp]

module Color : sig
  type t = color [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t

  (** The number of valid colors. *)
  val count : int

  (** Integer representation of white. *)
  val white : int

  (** Integer representation of black. *)
  val black : int

  (** [of_int_exn i] returns the color associated with integer [i].
      Raises [Invalid_argument] if [i] is out of range. *)
  val of_int_exn : int -> t

  (** [of_int i] returns the color associated with integer [i].
      Returns [None] if [i] is out of range. *)
  val of_int : int -> t option

  (** [to_int c] returns the integer associated with color [c]. *)
  val to_int : t -> int

  (** [opposite c] returns the opposite color of [c]. *)
  val opposite : t -> t
    
  (** [opposite_int c] returns the integer representation of the opposite
      color of [c]. *)
  val opposite_int : t -> int
end

(** The kind of a piece *)
type kind = Pawn | Knight | Bishop | Rook | Queen | King
[@@deriving compare, equal, hash, sexp]

module Kind : sig
  type t = kind [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t

  (** The number of valid pieces. *)
  val count : int

  (** Integer representation of pawn. *)
  val pawn : int

  (** Integer representation of knight. *)
  val knight : int

  (** Integer representation of bishop. *)
  val bishop : int

  (** Integer representation of rook. *)
  val rook : int

  (** Integer representation of queen. *)
  val queen : int

  (** Integer representation of king. *)
  val king : int

  (** [of_int_exn i] returns the piece associated with integer [i].
      Raises [Invalid_argument] if [i] is out of range. *)
  val of_int_exn : int -> t

  (** [of_int i] returns the piece associated with integer [i].
      Returns [None] if [i] is out of range. *)
  val of_int : int -> t option

  (** [to_int p] returns the integer associated with piece [p]. *)
  val to_int : t -> int
end

(** Representation of a Chess piece as an unboxed integer, which includes its
    color and kind. *)
type t = private int [@@deriving compare, equal, hash, sexp]

include Comparable.S with type t := t

(** The white pawn. *)
val white_pawn : t

(** The white knight. *)
val white_knight : t

(** The white bishop. *)
val white_bishop : t

(** The white rook. *)
val white_rook : t

(** The white queen. *)
val white_queen : t

(** The white king. *)
val white_king : t

(** The black pawn. *)
val black_pawn : t

(** The black knight. *)
val black_knight : t

(** The black bishop. *)
val black_bishop : t

(** The black rook. *)
val black_rook : t

(** The black queen. *)
val black_queen : t

(** The black king. *)
val black_king : t

(** [color p] returns the corresponding color of piece [p]. *)
val color : t -> color

(** [kind p] returns the corresponding kind of piece [p]. *)
val kind : t -> kind

(** [create color kind] creates a piece with color [color] and kind [kind]. *)
val create : color -> kind -> t

(** [is_white p] returns [true] if [p] is a white piece. *)
val is_white : t -> bool

(** [is_black p] returns [true] if [p] is a black piece. *)
val is_black : t -> bool

(** [is_pawn p] returns [true] if [p] is a pawn. *)
val is_pawn : t -> bool

(** [is_knight p] returns [true] if [p] is a knight. *)
val is_knight : t -> bool

(** [is_bishop p] returns [true] if [p] is a bishop. *)
val is_bishop : t -> bool

(** [is_rook p] returns [true] if [p] is a rook. *)
val is_rook : t -> bool

(** [is_queen p] returns [true] if [p] is a queen. *)
val is_queen : t -> bool

(** [is_king p] returns [true] if [p] is a king. *)
val is_king : t -> bool

(** [of_int_exn i] creates the representation of a piece from integer [i].
    Bits 0 through 3 must form an integer within the range [\[0,5\]], and
    represents the piece's kind. Bit 4 represents the color with [0] being
    white and [1] being black. All other bits must be zero. In any other
    case, [Invalid_argument] is raised. *)
val of_int_exn : int -> t

(** [of_int_exn i] creates the representation of a piece from integer [i].
    Bits 0 through 3 must form an integer within the range [\[0,5\]], and
    represents the piece's kind. Bit 4 represents the color with [0] being
    white and [1] being black. All other bits must be zero. In any other
    case, [None] is returned. *)
val of_int : int -> t option

(** [to_int p] returns the integer representation of piece [p]. *)
val to_int : t -> int

(** [of_fen_exn c] returns the corresponding piece given a character [c].
    [c] is expected to follow from FEN notation, shown as regex below:

    [(P|N|B|R|Q|K)] represents the white pieces, while [(p|n|b|r|q|k)]
    represents the black pieces

    All other inputs will raise [Invalid_argument]. *)
val of_fen_exn : char -> t

(** [of_fen c] returns the corresponding piece given a character [c].
    [c] is expected to follow from FEN notation, shown as regex below:

    [(P|N|B|R|Q|K)] represents the white pieces, while [(p|n|b|r|q|k)]
    represents the black pieces

    All other inputs will return [None]. *)
val of_fen : char -> t option

(** [to_fen p] returns the corresponding FEN notation of piece [p]. *)
val to_fen : t -> char
