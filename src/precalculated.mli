(** [pawn sq color] returns a bitboard of potentially valid squares for a
    pawn of color [color] at square [sq]. *)
val pawn : Square.t -> Piece.color -> Bitboard.t

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

(** [castle color side] returns a bitboard of potentially valid castling
    squares for color [color] and side [side]. *)
val castle : Piece.color -> [`queen | `king] -> Bitboard.t
