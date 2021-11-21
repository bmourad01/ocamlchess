open Base

(** Representation of a chess position, compatible with FEN notation.

    [white] and [black] represents piece placement as bitboards, with respect
    to the color of the pieces.

    [pawn], [knight], [bishop], [rook], [queen], and [king] represent piece
    placement as bitboards, with respect to the kinds of the pieces.

    [active] is the active color (e.g. which player is to move next).

    [castle] represents castling rights for each color.

    [en_passant] is the square, if any, of the en passant target square. If
    it exists, then a pawn has just made a two-square move, and it is thus
    the square "behind" the pawn.

    [halfmove] is the number of halfmoves since the last capture or pawn
    advance, used for the fifty-move rule.

    [fullmove] is the number of full moves that have been completed. *)
type t =
  { white: Bitboard.t
  ; black: Bitboard.t
  ; pawn: Bitboard.t
  ; knight: Bitboard.t
  ; bishop: Bitboard.t
  ; rook: Bitboard.t
  ; queen: Bitboard.t
  ; king: Bitboard.t
  ; active: Piece.color
  ; castle: Castling_rights.t
  ; en_passant: Square.t option
  ; halfmove: int
  ; fullmove: int }
[@@deriving compare, equal, hash, sexp]

(** [board_of_color pos c] returns the bitboard of color [c] from position
    [pos]. *)
val board_of_color : t -> Piece.color -> Bitboard.t

(** [board_of_color pos k] returns the bitboard of kind [k] from position
    [pos]. *)
val board_of_kind : t -> Piece.kind -> Bitboard.t

(** [board_of_piece pos p] returns the bitboard of piece [p] from position
    [pos]. *)
val board_of_piece : t -> Piece.t -> Bitboard.t

(** [find_color pos c] returns a list of square-kind pairs where pieces of
    color [c] occupy squares on position [pos]. *)
val find_color : t -> Piece.color -> (Square.t * Piece.kind) list

(** [find_kind pos k] returns a list of square-color pairs where pieces of
    kind [k] occupy squares on position [pos]. *)
val find_kind : t -> Piece.kind -> (Square.t * Piece.color) list

(** [find_piece pos p] returns a list of squares where pieces matching [p]
    occupy squares on position [pos]. *)
val find_piece : t -> Piece.t -> Square.t list

(** [piece_at_square pos sq] returns the piece, if any, occupying the square
    [sq] on position [pos]. *)
val piece_at_square : t -> Square.t -> Piece.t option
