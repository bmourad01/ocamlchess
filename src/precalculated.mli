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

(** [castle rights c side] returns a bitboard of potentially valid castling
    squares w.r.t. [rights] for color [c] and side [side]. *)
val castle :
  Castling_rights.t -> Piece.color -> Castling_rights.side -> Bitboard.t
