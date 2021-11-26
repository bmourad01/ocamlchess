open Base

(** Representation of a chess position. *)
type t [@@deriving compare, equal, hash, sexp]

(** [white pos] returns the bitboard representing all squares occupied by
    white pieces in position [pos]. *)
val white : t -> Bitboard.t

(** [black pos] returns the bitboard representing all squares occupied by
    black pieces in position [pos]. *)
val black : t -> Bitboard.t

(** [pawn pos] returns the bitboard representing all squares occupied by
    pawns in position [pos]. *)
val pawn : t -> Bitboard.t

(** [knight pos] returns the bitboard representing all squares occupied by
    knights in position [pos]. *)
val knight : t -> Bitboard.t

(** [bishop pos] returns the bitboard representing all squares occupied by
    bishops in position [pos]. *)
val bishop : t -> Bitboard.t

(** [rook pos] returns the bitboard representing all squares occupied by
    rooks in position [pos]. *)
val rook : t -> Bitboard.t

(** [queen pos] returns the bitboard representing all squares occupied by
    queens in position [pos]. *)
val queen : t -> Bitboard.t

(** [king pos] returns the bitboard representing all squares occupied by
    kings in position [pos]. *)
val king : t -> Bitboard.t

(** [active pos] returns the active color (whose turn it is to move) for
    position [pos]. *)
val active : t -> Piece.color

(** [castle pos] returns the castling rights for position [pos]. *)
val castle : t -> Castling_rights.t

(** [en_passant pos] returns the square, if any, of the en passant target
    square. If it exists, then a pawn has just made a two-square move, and it
    is thus the square "behind" the pawn. *)
val en_passant : t -> Square.t option

(** [halfmove pos] returns the number of halfmoves since the last capture or
    pawn advance. Used for the fifty-move rule. *)
val halfmove : t -> int

(** [fullmove pos] returns then number of full moves that have been made. *)
val fullmove : t -> int

(** [board_of_color pos c] returns the bitboard of color [c] from position
    [pos]. *)
val board_of_color : t -> Piece.color -> Bitboard.t

(** [active_board pos] returns the bitboard for all the pieces of the active
    color. *)
val active_board : t -> Bitboard.t

(** [board_of_color pos k] returns the bitboard of kind [k] from position
    [pos]. *)
val board_of_kind : t -> Piece.kind -> Bitboard.t

(** [board_of_piece pos p] returns the bitboard of piece [p] from position
    [pos]. *)
val board_of_piece : t -> Piece.t -> Bitboard.t

(** [is_en_passant t sq] returns [true] if an en passant square exists
    in [pos] and it is equal to [sq]. *)
val is_en_passant : t -> Square.t -> bool

(** [find_color pos c] returns a list of square-kind pairs where pieces of
    color [c] occupy squares on position [pos]. *)
val find_color : t -> Piece.color -> (Square.t * Piece.kind) list

(** [find_active pos] returns a list of square-kind pairs where pieces of
    the active color occupy squares on position [pos]. *)
val find_active : t -> (Square.t * Piece.kind) list

(** [find_kind pos k] returns a list of square-color pairs where pieces of
    kind [k] occupy squares on position [pos]. *)
val find_kind : t -> Piece.kind -> (Square.t * Piece.color) list

(** [find_piece pos p] returns a list of squares where pieces matching [p]
    occupy squares on position [pos]. *)
val find_piece : t -> Piece.t -> Square.t list

(** [piece_at_square pos sq] returns the piece, if any, occupying the square
    [sq] on position [pos]. *)
val piece_at_square : t -> Square.t -> Piece.t option

(** [all_pieces pos] returns a list of square-piece pairs for each occupied
    square on position [pos]. *)
val all_pieces : t -> (Square.t * Piece.t) list

(** This submodule provides compatibility with FEN (Forsyth-Edwards
    Notation). This notation is designed to give a compact ASCII
    representation of any chess position. *)
module Fen : sig
  (** String representation of the starting position. *)
  val start : string

  (** [of_string_exn s] attempts to parse a FEN string [s] into a valid
      position. Raises [Invalid_argument] if [s] is not a valid FEN string. *)
  val of_string_exn : string -> t

  (** [of_string_exn s] attempts to parse a FEN string [s] into a valid
      representation. Returns [None] if [s] is not a valid FEN string. *)
  val of_string : string -> t option

  (** [to_string fen] returns a string representation of [fen]. *)
  val to_string : t -> string
end

(** The starting position. *)
val start : t

(** This submodule provides helper functions related to generating attacked
    squares for a particular color. *)
module Attacks : sig
  (** [pawn pos c] returns the bitboard of attacked squares by pawns of color
      [c], according to position [pos]. *)
  val pawn : t -> Piece.color -> Bitboard.t

  (** [knight pos c] returns the bitboard of attacked squares by knights of
      color [c], according to position [pos]. *)
  val knight : t -> Piece.color -> Bitboard.t

  (** [bishop pos c ~king_danger] returns the bitboard of attacked squares by
      bishops of color [c], according to position [pos]. [king_danger]
      indicates whether the sliding bishop attack should ignore the enemy
      king. By default, it is [false]. *)
  val bishop : ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [rook pos c ~king_danger] returns the bitboard of attacked squares by
      rooks of color [c], according to position [pos]. [king_danger]
      indicates whether the sliding rook attack should ignore the enemy king.
      By default, it is [false]. *)
  val rook : ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [queen pos c ~king_danger] returns the bitboard of attacked squares by
      queens of color [c], according to position [pos]. [king_danger]
      indicates whether the sliding queen attack should ignore the enemy
      king. By default, it is [false]. *)
  val queen : ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [king pos c] returns the bitboard of attacked squares by kings of color
      [c], according to position [pos]. *)
  val king : t -> Piece.color -> Bitboard.t

  (** [all pos c ~king_danger] returns the bitboard of attacked squares by
      all pieces of color [c], according to position [pos]. [king_danger]
      indicates whether the sliding attacks should ignore the enemy king. By
      default, it is [false]. *)
  val all : ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [sliding pos c ~king_danger] returns the bitboard of attacked squares
      by all sliding pieces of color [c], according to position [pos].
      [king_danger] indicates whether these attacks should ignore the enemy
      king. By default, it is [false]. *)
  val sliding : ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [non_sliding pos c] returns the bitboard of attacked squares by all
      non-sliding pieces of color [c], according to position [pos]. *)
  val non_sliding : t -> Piece.color -> Bitboard.t
end

(** This module provides helper functions related to generating legal moves
    from a position. *)
module Moves : sig
  (** [pinned_pieces pos] returns the bitboard representing the squares occupied
      by all pinned pieces for the active color. *)
  val pinned_pieces : t -> Bitboard.t
end
