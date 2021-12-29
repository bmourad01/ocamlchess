open Base

(** Representation of a chess position. *)
type t [@@deriving compare, equal, sexp]

include Comparable.S with type t := t

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

(** [enemy pos] returns the opposite of the active color for position [pos]. *)
val enemy : t -> Piece.color

(** [all_board pos] returns the bitboard of all occupied squares for position
    [pos]. *)
val all_board : t -> Bitboard.t  

(** [board_of_color pos c] returns the bitboard of color [c] from position
    [pos]. *)
val board_of_color : t -> Piece.color -> Bitboard.t

(** [active_board pos] returns the bitboard for all the pieces of the active
    color. *)
val active_board : t -> Bitboard.t

(** [enemy_board pos] returns the bitboard for all the pieces of the enemy
    color. *)
val enemy_board : t -> Bitboard.t

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

(** This submodule is concerned with checking the validity of a given
    position (it may not be exhaustive!). *)
module Valid : sig
  (** The possible errors that can arise from validating a position. *)
  module Error : sig
    type t =
      | Invalid_number_of_kings of Piece.color * int
      | Kings_not_separated
      | Inactive_in_check of Piece.color
      | Invalid_number_of_checkers of Piece.color * int
      | Invalid_two_checkers of Piece.color * Piece.kind * Piece.kind
      | Invalid_number_of_pawns of Piece.color * int
      | Pawns_in_back_rank of Piece.color
      | Missing_pawn_en_passant of Piece.color
      | Invalid_en_passant_square of Square.t
      | Invalid_extra_pieces of Piece.color * int
      | Invalid_number_of_pieces of Piece.color * int
      | Invalid_castling_rights of Piece.color * Piece.kind
      | En_passant_wrong_halfmove
      | Invalid_halfmove
      | Invalid_fullmove

    (** [to_string err] converts [err] into a human-readable string. *)
    val to_string : t -> string
  end

  (** Validation error. *)
  type error = Error.t

  (** [check pos] will return [Ok ()] if the position is valid (i.e. reachable
      from the starting position), and [Error err] otherwise. *)
  val check : t -> (unit, error) Result.t
end

(** This submodule provides compatibility with FEN (Forsyth-Edwards
    Notation). This notation is designed to give a compact ASCII
    representation of any chess position. *)
module Fen : sig
  (** The possible errors that can arise from parsing a FEN string. *)
  module Error : sig
    type t =
      | Invalid_number_of_ranks of int
      | Invalid_file_increment of int * Square.t
      | Rank_full of int
      | Invalid_piece_symbol of char * Square.t
      | Unspecified_squares of int * int
      | Invalid_active_color of string
      | Invalid_castling_rights of string
      | Invalid_en_passant of string
      | Invalid_halfmove of string
      | Invalid_fullmove of string
      | Invalid_position of Valid.error
      | Invalid_number_of_sections of int

    (** [to_string err] converts [err] into a human-readable string. *)
    val to_string : t -> string
  end

  type error = Error.t

  (** String representation of the starting position. *)
  val start : string

  (** [of_string_exn s ~validate] attempts to parse a FEN string [s] into a
      position. Returns [Ok pos] if [s] is a syntactically valid FEN string,
      and [Error err] otherwise. If [validate] is [true], then the position
      is checked for legality, returning [Error err] upon failure. By default,
      it is [true]. Use of invalid chess positions (e.g. with the move
      generator) may result in undefined behavior. *)
  val of_string : ?validate:bool -> string -> (t, error) Result.t

  (** [of_string_exn s ~validate] attempts to parse a FEN string [s] into a
      position. Raises [Invalid_argument] if [s] is not a syntactically valid
      FEN string. If [validate] is [true], then the position is checked for
      legality, raising [Invalid_argument] upon failure. By default, it is
      [true]. Use of invalid chess positions (e.g. with the move generator)
      may result in undefined behavior. *)
  val of_string_exn : ?validate:bool -> string -> t

  (** [to_string fen] returns a string representation of [fen]. *)
  val to_string : t -> string
end

(** The starting position. *)
val start : t

(** This submodule provides helper functions related to generating attacked
    squares for a particular color. *)
module Attacks : sig
  (** [pawn pos c ~ignore_same] returns the bitboard of attacked squares by
      pawns of color [c], according to position [pos]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val pawn : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [knight pos c ~ignore_same] returns the bitboard of attacked squares by
      knights of color [c], according to position [pos]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val knight : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [bishop pos c ~ignore_same ~king_danger] returns the bitboard of attacked
      squares by bishops of color [c], according to position [pos].
      [king_danger] indicates whether the sliding bishop attack should ignore
      the enemy king. By default, it is [false]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val bishop :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [rook pos c ~ignore_same ~king_danger] returns the bitboard of attacked
      squares by rooks of color [c], according to position [pos]. [king_danger]
      indicates whether the sliding rook attack should ignore the enemy king.
      By default, it is [false]. If [ignore_same] is [false], then pieces of
      color [c] are not excluded from the set of attacked squares. By default,
      it is [true]. *)
  val rook :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [queen pos c ~ignore_same ~king_danger] returns the bitboard of attacked
      squares by queens of color [c], according to position [pos].
      [king_danger] indicates whether the sliding queen attack should ignore
      the enemy king. By default, it is [false]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true].*)
  val queen :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [king pos c ~ignore_same] returns the bitboard of attacked squares by
      kings of color [c], according to position [pos]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val king : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [all pos c ~ignore_same ~king_danger] returns the bitboard of attacked
      squares by all pieces of color [c], according to position [pos].
      [king_danger] indicates whether the sliding attacks should ignore the
      enemy king. By default, it is [false]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val all :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [sliding pos c ~ignore_same ~king_danger] returns the bitboard of
      attacked squares by all sliding pieces of color [c], according to
      position [pos]. [king_danger] indicates whether these attacks should
      ignore the enemy king. By default, it is [false]. If [ignore_same] is
      [false], then pieces of color [c] are not excluded from the set of
      attacked squares. By default, it is [true]. *)
  val sliding :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [non_sliding pos c ~ignore_same] returns the bitboard of attacked squares
      by all non-sliding pieces of color [c], according to position [pos].
      If [ignore_same] is [false], then pieces of color [c] are not excluded
      from the set of attacked squares. By default, it is [true]. *)
  val non_sliding : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t
end

(** [in_check pos] returns [true] if the active player for position [pos] is
    currently in check. *)
val in_check : t -> bool

(** A legal move. *)
type legal_move

module Legal_move : sig
  (** The actual move that was made. *)
  val move : legal_move -> Move.t

  (** The resulting position. *)
  val position : legal_move -> t

  (** Decomposes the structure into its constituents. *)
  val decomp : legal_move -> Move.t * t

  (** A legal move. *)
  type t = legal_move [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t
end

(** A list of legal moves for a position. *)
type legal_moves

module Legal_moves : sig
  (** The list of legal moves. *)
  val moves : legal_moves -> legal_move list

  (** The parent position. *)
  val position : legal_moves -> t

  (** Decomposes the structure into its constituents. *)
  val decomp : legal_moves -> legal_move list * t

  (** A list of legal moves for a position. *)
  type t = legal_moves [@@deriving compare, equal, sexp]

  include Comparable.S with type t := t
end

(** [legal_moves pos] returns a list of move-position pairs, which represents
    all of the legal moves for the active color of position [pos]. It is
    assumed that [pos] is reachable from the starting position. May raise
    if [pos] is not valid. No particular order is guaranteed for the resulting
    list. *)
val legal_moves : t -> legal_moves