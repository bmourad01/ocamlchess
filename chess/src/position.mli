(** This module implements the data strucutres and interfaces associated with
    Chess positions and legal move generation. *)

(** Representation of a chess position. *)
type t [@@deriving compare, equal, sexp]

include Base.Comparable.S with type t := t

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

(** [pinned pos c] returns the bitboard representing all pinned pieces for
    the king of color [c] in position [pos]. *)
val pinned : t -> Piece.color -> Bitboard.t

(** [pinners pos c] returns the bitboard for pieces of color [c] that are
    pinning an enemy piece to the king in position [pos]. *)
val pinners : t -> Piece.color -> Bitboard.t

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

(** [hash pos] returns the Zobrist hash of the position. *)
val hash : t -> Zobrist.key

(** [pawn_hash pos] returns the Zobrist hash of the pawn structure. *)
val pawn_hash : t -> Zobrist.key

(** [same_hash pos1 pos2] returns [true] if [pos1] and [pos2] have the same
    Zobrist hash. *)
val same_hash : t -> t -> bool

(** [inactive pos] returns the opposite of the active color for position [pos]. *)
val inactive : t -> Piece.color

(** [all_board pos] returns the bitboard of all occupied squares for position
    [pos]. *)
val all_board : t -> Bitboard.t  

(** [board_of_color pos c] returns the bitboard of color [c] from position
    [pos]. *)
val board_of_color : t -> Piece.color -> Bitboard.t

(** [active_board pos] returns the bitboard for all the pieces of the active
    color. *)
val active_board : t -> Bitboard.t

(** [inactive_board pos] returns the bitboard for all the pieces of the inactive
    color. *)
val inactive_board : t -> Bitboard.t

(** [board_of_kind pos k] returns the bitboard of kind [k] from position
    [pos]. *)
val board_of_kind : t -> Piece.kind -> Bitboard.t

(** [board_of_piece pos p] returns the bitboard of piece [p] from position
    [pos]. *)
val board_of_piece : t -> Piece.t -> Bitboard.t

(** [is_en_passant_square pos sq] returns [true] if an en passant square exists
    in [pos] and it is equal to [sq]. *)
val is_en_passant_square : t -> Square.t -> bool

(** [en_passant_pawn pos] returns the square of the pawn that just made a
    double push, if any. *)
val en_passant_pawn : t -> Square.t option

(** [collect_color pos c] returns a list of square-kind pairs where pieces of
    color [c] occupy squares on position [pos]. *)
val collect_color : t -> Piece.color -> (Square.t * Piece.kind) list

(** [collect_active pos] returns a list of square-kind pairs where pieces of
    the active color occupy squares on position [pos]. *)
val collect_active : t -> (Square.t * Piece.kind) list

(** [collect_inactive pos] returns a list of square-kind pairs where pieces of
    the opponent's color occupy squares on position [pos]. *)
val collect_inactive : t -> (Square.t * Piece.kind) list

(** [collect_kind pos k] returns a list of square-color pairs where pieces of
    kind [k] occupy squares on position [pos]. *)
val collect_kind : t -> Piece.kind -> (Square.t * Piece.color) list

(** [collect_piece pos p] returns a list of squares where pieces matching [p]
    occupy squares on position [pos]. *)
val collect_piece : t -> Piece.t -> Square.t list

(** [piece_at_square pos sq] returns the piece, if any, occupying the square
    [sq] on position [pos]. *)
val piece_at_square : t -> Square.t -> Piece.t option

(** [piece_at_square_exn pos sq] returns the piece, if any, occupying the
    square [sq] on position [pos]. Raises [Invalid_argument] otherwise. *)
val piece_at_square_exn : t -> Square.t -> Piece.t

(** [collect_all pos] returns a list of square-piece pairs for each occupied
    square on position [pos]. *)
val collect_all : t -> (Square.t * Piece.t) list

(** [has_non_pawn_material pos c] returns [true] if the player of color [c]
    has non-pawn material in position [pos]. *)
val has_non_pawn_material : t -> Piece.color -> bool

(** This submodule is concerned with checking the validity of a given
    position (it may not be exhaustive!). *)
module Valid : sig
  (** The possible errors that can arise from validating a position. *)
  module Error : sig
    type t =
      | Empty_board
      | Full_board
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

    (** [pp ppf err] pretty-prints [err] to formatter [ppf]. *)
    val pp : Format.formatter -> t -> unit

    (** [to_string err] converts [err] into a human-readable string. *)
    val to_string : t -> string
  end

  (** Validation error. *)
  type error = Error.t

  (** [check pos] will return [Ok ()] if the position is valid (i.e. reachable
      from the starting position), and [Error err] otherwise. *)
  val check : t -> (unit, error) result
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

    (** [pp ppf err] pretty-prints [err] to formatter [ppf]. *)
    val pp : Format.formatter -> t -> unit

    (** [to_string err] converts [err] into a human-readable string. *)
    val to_string : t -> string
  end

  type error = Error.t

  (** String representation of the starting position. *)
  val start : string

  (** [of_string s ~validate] attempts to parse a FEN string [s] into a
      position. Returns [Ok pos] if [s] is a syntactically valid FEN string,
      and [Error err] otherwise. If [validate] is [true], then the position
      is checked for legality, returning [Error err] upon failure. By default,
      it is [true]. Use of invalid chess positions (e.g. with the move
      generator) may result in undefined behavior. *)
  val of_string : ?validate:bool -> string -> (t, error) result

  (** Same as [of_string], but raises [Invalid_argument] if an error is
      encountered. *)
  val of_string_exn : ?validate:bool -> string -> t

  (** [pp ppf pos] pretty-prints the FEN representation of [pos]. *)
  val pp : Format.formatter -> t -> unit

  (** [to_string pos] returns the FEN representation of [pos]. *)
  val to_string : t -> string
end

(** Alias for [Fen.pp]. *)
val pp : Format.formatter -> t -> unit

(** The starting position. *)
val start : t

(** This submodule provides helper functions related to generating attacked
    squares for a particular color.

    The following common parameters among each function are described as
    follows:

    - [ignore_same]: if this is [false] then the pieces of the same color
      as the attacker in question are included in the set of attacked
      squares. By default, it is [true].

    - [king_danger]: if this is [true], then sliding attacks (bishop, rook,
      and queen) will pretend that the enemy king's square is unoccupied,
      thus the attacks will "see through" the king. By default, it is [false].
*)
module Attacks : sig
  (** [pawn pos c ~ignore_same] returns the set of attacked squares by
      pawns of color [c] for position [pos]. *)
  val pawn : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [knight pos c ~ignore_same] returns the set of attacked squares by
      knights of color [c] for position [pos]. *)
  val knight : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [bishop pos c ~ignore_same ~king_danger] returns the set of attacked
      squares by bishops of color [c] for position [pos]. *)
  val bishop :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [rook pos c ~ignore_same ~king_danger] returns the set of attacked
      squares by rooks of color [c] for position [pos]. *)
  val rook :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [queen pos c ~ignore_same ~king_danger] returns the set of attacked
      squares by queens of color [c] for position [pos]. *)
  val queen :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [king pos c ~ignore_same] returns the set of attacked squares by kings
      of color [c] for position [pos]. *)
  val king : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t

  (** [all pos c ~ignore_same ~king_danger] returns the set of attacked
      squares by all pieces of color [c] for position [pos]. *)
  val all :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [sliding pos c ~ignore_same ~king_danger] returns the set of attacked
      squares by all sliding pieces of color [c] for position [pos]. *)
  val sliding :
    ?ignore_same:bool -> ?king_danger:bool -> t -> Piece.color -> Bitboard.t

  (** [non_sliding pos c ~ignore_same] returns the set of attacked squares
      by all non-sliding pieces of color [c] for position [pos]. *)
  val non_sliding : ?ignore_same:bool -> t -> Piece.color -> Bitboard.t
end

(** Information about threatened pieces. *)
type threats [@@deriving compare, equal, sexp]

(** This submodule provides a way to calculate information about threatened
    pieces. *)
module Threats : sig
  (** [get pos c] calculates the threats of position [pos] coming from the
      player of color [c]. *)
  val get : t -> Piece.color -> threats

  (** [count t] returns the total number of threats from [t]. *)
  val count : threats -> int

  (** [color t] returns the color of the pieces that are making the threats. *)
  val color : threats -> Piece.color

  (** [pawn t] returns the bitboard of minor and major pieces threatened
      by pawns. *)
  val pawn : threats -> Bitboard.t

  (** [minor t] returns the bitboard of major pieces threatened by minor
      pieces. *)
  val minor : threats -> Bitboard.t

  (** [rook t] returns the bitboard of queens that are threatened by rooks. *)
  val rook : threats -> Bitboard.t

  type t = threats [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t
end

(** [checkers pos] returns the bitboard of inactive pieces that are checking
    the active king of position [pos]. *)
val checkers : t -> Bitboard.t

(** [in_check pos] is shorthand for [Bitboard.(checkers pos <> 0)]. *)
val in_check : t -> bool

(** [is_insufficient_material pos] returns [true] if the game may result in a
    draw due to insufficient material on both sides. Insufficient material
    is defined as both sides not having sufficient material to deliver
    checkmate. *)
val is_insufficient_material : t -> bool

(** A position resulting from a legal move. *)
type child

module Child : sig
  (** [same x y] returns [true] if [x] and [y] refer to the same move.
      This is determined by comparing the hashes of both the [parent] and 
      [new_position] for [x] and [y], respectively. *)
  val same : child -> child -> bool

  (** The actual move that was made. *)
  val move : child -> Move.t

  (** [is_move child m] returns [true] if [m] is the move that was made for
      [child] *)
  val is_move : child -> Move.t -> bool

  (** The parent position that this move was applied to. *)
  val parent : child -> t

  (** The resulting position. *)
  val self : child -> t

  (** Returns [true] if the move was a capture. *)
  val is_capture : child -> bool

  (** Returns [true] if the move was a castle. *)
  val is_castle : child -> bool

  (** Returns [true] if the move was an en passant capture. *)
  val is_en_passant : child -> bool

  (** The kind of piece that was captured, if any. *)
  val capture : child -> Piece.kind option

  (** The square of the piece that was captured, if any. Covers the case of
      en passant captures, where the destination square is not the square
      of the captured piece. *)
  val capture_square : child -> Square.t option

  (** If the move was a castle, then returns the side that the castling was
      performed on. Otherwise, returns [None]. *)
  val castle_side : child -> Castling_rights.side option

  (** Returns [true] if the move gives check to the opponent. *)
  val gives_check : child -> bool

  (** Returns the bitboard of enemy pieces (relative to the parent position)
      that are under threat as a result of the move. *)
  val new_threats : child -> Bitboard.t

  type t = child
end

(** Returns the list of legal moves without applying them to the
    position. No particular order is guaranteed. *)
val legal_moves : t -> Move.t list

(** Returns the list of legal moves that are captures. *)
val capture_moves : t -> Move.t list

(** Returns the list of legal moves that are promotions. *)
val promotion_moves : t -> Move.t list

(** Returns the list of legal moves that are not captures or promotions. *)
val quiet_moves : t -> Move.t list

(** [gives_check pos m] returns [true] if the move [m] gives check for
    position [pos].

    If [Move.src m] does not point to a piece of the active color, then
    the result is [false].
*)
val gives_check : t -> Move.t -> bool

(** [children pos] returns the result of applying all legal moves of
    position [pos].

    It is assumed that [pos] is reachable from the starting  position.
    May raise if this assumption is violated. No particular order is
    guaranteed for the resulting list.
*)
val children : t -> child list

(** Returns the list of children that are captures. *)
val capture_children : t -> child list

(** Returns the list of children that are promotions. *)
val promotion_children : t -> child list

(** Returns the list of children that are not captures or promotions. *)
val quiet_children : t -> child list

(** [make_move pos m] applies move [m] to position [pos]. If [m] is not a
    legal move, or if [pos] is an illegal position, then [None] is returned.
    Otherwise, the result is the legal move, which contains the new
    position. *)
val make_move : t -> Move.t -> child option

(** Like [make_move], but raises [Invalid_argument] for illegal moves or
    positions. *)
val make_move_exn : t -> Move.t -> child

(** Same as [Unsafe.null_move], but will raise [Invalid_argument] if the
    position is in check. *)
val null_move_exn : t -> t

(** This submodule exposes some unsafe functionality for manipulating or
    querying the position. They are unsafe because no checks are performed
    for the legality of the inputs.
*)
module Unsafe : sig
  (** [make_move pos m] applies move [m] to position [pos]. No checks for
      legality are performed. *)
  val make_move : t -> Move.t -> child

  (** [null_move pos] switches the active player of [pos], pretending that
      no move was played.

      If the active player of [pos] is in check, then the resulting 
      position is illegal, and any behavior thereafter is undefined.
  *)
  val null_move : t -> t

  (** [is_en_passant pos m] returns [true] if move [m] is an en-passant
      capture, given position [pos]. *)
  val is_en_passant : t -> Move.t -> bool

  (** [is_capture pos m] returns [true] if move [m] is a capture, given
      position [pos]. *)
  val is_capture : t -> Move.t -> bool

  (** [is_castle pos m] returns [true] if move [m] is a castling move,
      given position [pos]. *)
  val is_castle : t -> Move.t -> bool
end

(** Implements SAN (Standard Algebraic Notation). *)
module San : sig
  (** [pp ppf legal] pretty-prints the [child] in SAN to formatter [ppf]. *)
  val pp : Format.formatter -> child -> unit

  (** [of_child child] returns a string representing the [child] move in SAN. *)
  val of_child : child -> string

  (** [of_move pos m] returns a string representing move [m] for position
      [pos], if it is legal. *)
  val of_move : t -> Move.t -> string option

  (** Same as [of_move], but raises [Invalid_argument] if the move is not
      legal. *)
  val of_move_exn : t -> Move.t -> string

  (** [of_string s pos] returns the legal move of string [s] given the
      position [pos], if it exists. *)
  val of_string : string -> t -> child option
end
