(** This module implements an interface for looking up Chess-relevant,
    precomputed data, such as attack tables. *)

(** Directional masks, from white's pespective. *)
module Mask : sig
  (** [east sq] returns the mask of all squares east of [sq]. *)
  val east : Square.t -> Bitboard.t

  (** [west sq] returns the mask of all squares west of [sq]. *)
  val west : Square.t -> Bitboard.t

  (** [north sq] returns the mask of all squares north of [sq]. *)
  val north : Square.t -> Bitboard.t

  (** [south sq] returns the mask of all squares south of [sq]. *)
  val south : Square.t -> Bitboard.t

  (** [neast sq] returns the mask of all squares northeast of [sq]. *)
  val neast : Square.t -> Bitboard.t

  (** [nwest sq] returns the mask of all squares northwest of [sq]. *)
  val nwest : Square.t -> Bitboard.t

  (** [seast sq] returns the mask of all squares southeast of [sq]. *)
  val seast : Square.t -> Bitboard.t

  (** [swest sq] returns the mask of all squares southwest of [sq]. *)
  val swest : Square.t -> Bitboard.t
end

(** [pawn_advance sq color] returns a bitboard of potentially valid squares
    for a pawn of color [color] at square [sq] to advance without capturing a
    piece.. *)
val pawn_advance : Square.t -> Piece.color -> Bitboard.t

(** [pawn_capture sq color] returns a bitboard of potentially valid squares
    for a pawn of color [color] at square [sq] to capture a piece. *)
val pawn_capture : Square.t -> Piece.color -> Bitboard.t

(** [knight sq] returns a bitboard of potentially valid squares for a knight
    at square [sq]. *)
val knight : Square.t -> Bitboard.t

(** [bishop sq occupied] returns a bitboard of potentially valid squares for
    a bishop at square [sq] with the occupied squares [occupied]. *)
val bishop : Square.t -> Bitboard.t -> Bitboard.t

(** [rook sq occupied] returns a bitboard of potentially valid squares for a
    rook at square [sq] with the occupied squares [occupied]. *)
val rook : Square.t -> Bitboard.t -> Bitboard.t

(** [queen sq occupied] returns a bitboard of potentially valid squares for a
    queen at square [sq] with the occupied squares [occupied]. *)
val queen : Square.t -> Bitboard.t -> Bitboard.t

(** [king sq] returns a bitboard of potentially valid squares for a king at
    square [sq]. *)
val king : Square.t -> Bitboard.t

(** [attacks sq occupied c k] returns the attacks bitboard at square [sq]
    for the piece of color [c] and kind [k], given the set of [occupied]
    squares. *)
val attacks : Square.t -> Bitboard.t -> Piece.color -> Piece.kind -> Bitboard.t

(** [castle cr c s] returns the corresponding squares for color [c] to castle
    on side [s], with respect to castling rights [cr]. These squares only
    cover movement for the king. *)
val castle :
  Castling_rights.t ->
  Piece.color ->
  Castling_rights.side ->
  Bitboard.t

(** [between x y] returns the bitboard of all squares between [x] and [y]
    w.r.t. a sliding move. *)
val between : Square.t -> Square.t -> Bitboard.t

(** [line x y] returns the bitboard representing the entire line (w.r.t.
    sliding moves) that intersects with squares [x] and [y]. *)
val line : Square.t -> Square.t -> Bitboard.t

(** [mvv_lva victim attacker] computes an ordering for the most valuable
    [victim] and least valuable [attacker]. Higher values are better . *)
val mvv_lva : Piece.kind -> Piece.kind -> int

(** [king_area sq c] returns the mask of the king's safety area at square
    [sq] for the player of color [c]. *)
val king_area : Square.t -> Piece.color -> Bitboard.t

(** [passed_pawns sq c] returns the mask for recognizing a potential passed
    pawn at square [sq] and of color [c]. *)
val passed_pawns : Square.t -> Piece.color -> Bitboard.t

(** [forward_ranks sq c] returns the ranks that are ahead of square [sq]
    for pieces of color [c]. *)
val forward_ranks : Square.t -> Piece.color -> Bitboard.t

(** [forward_files sq c] returns the files that are ahead of square [sq]
    for pieces of color [c]. *)
val forward_files : Square.t -> Piece.color -> Bitboard.t

(** [outpost_ranks c] returns the knight and bishop outpost ranks for
    pieces of color [c]. *)
val outpost_ranks : Piece.color -> Bitboard.t

(** [outpost_squares sq c] returns the knight and bishop outpost squares
    at square [c] for pieces of color [c]. *)
val outpost_squares : Square.t -> Piece.color -> Bitboard.t

(** [connected_pawns sq c] returns the mask for recognizing connected
    pawns at square [sq] for pawns of color [c]. *)
val connected_pawns : Square.t -> Piece.color -> Bitboard.t

(** [king_pawn_file_distance sq pawn] returns the distance, in files,
    between the king at square [sq] and the nearest pawn in the bitboard
    of pawns [pawn]. *)
val king_pawn_file_distance : Square.t -> Bitboard.t -> int
