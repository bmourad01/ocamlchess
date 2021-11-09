open Base

(** FEN string representation of the starting Chess position. *)
val start : string

(** Representation of a FEN configuration.

    [placement] represents the piece placement as a mapping from squares to
    pieces. If a square is not mapped, then it is empty.

    [active] is the active color (e.g. which player is to move next).

    [castle] represents castling rights for each color.

    [en_passant] is the square, if any, of the en passant target square. If
    it exists, then a pawn has just made a two-square move, and it is thus
    the square "behind" the pawn.

    [halfmove] is the number of halfmoves since the last capture or pawn
    advance, used for the fifty-move rule.

    [fullmove] is the number of full moves that have been completed. *)
type t =
  { placement: Piece.t Map.M(Square).t
  ; active: Piece.color
  ; castle: Castling_rights.t
  ; en_passant: Square.t option
  ; halfmove: int
  ; fullmove: int }
[@@deriving compare, equal, hash, sexp]

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Raises [Invalid_argument] if [s] is not a valid FEN
    string. *)
val of_string_exn : string -> t

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Returns [None] if [s] is not a valid FEN string. *)
val of_string : string -> t option

(** [create ()] constructs the starting position. *)
val create : unit -> t

(** [to_string fen] returns a string representation of [fen]. *)
val to_string : t -> string
