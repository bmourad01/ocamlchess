(** This module contains accessors for the random numbers used for generating
    Zobrist keys. They are the same as those used in the Polyglot adapter. *)

(** [piece c k sq] returns the random number associated with the piece of
    color [c] and kind [k] at square [sq]. *)
val piece : Piece.color -> Piece.kind -> Square.t -> int64

(** [en_passant sq] returns the random number associated with the en passant
    square [sq]. *)
val en_passant : Square.t -> int64

(** [castle c s] returns the random number associated with the castling rights
    for color [c] and side [s]. *)
val castle : Piece.color -> Castling_rights.side -> int64

(** [white_to_move] is the key associated with whether the active color is
    white. *)
val white_to_move : int64
