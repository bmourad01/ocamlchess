(** This module contains utilities for Zobrist hashing. The random numbers
    are the same as those used in the Polyglot chess adapter. *)

type key = int64 [@@deriving equal, compare, sexp]

(** [piece c k sq] returns the random number associated with the piece of
    color [c] and kind [k] at square [sq]. *)
val piece : Piece.color -> Piece.kind -> Square.t -> key

(** [en_passant sq] returns the random number associated with the en passant
    square [sq]. *)
val en_passant : Square.t -> key

(** [castle c s] returns the random number associated with the castling rights
    for color [c] and side [s]. *)
val castle : Piece.color -> Castling_rights.side -> key

(** [white_to_move] is the key associated with whether the active color is
    white. *)
val white_to_move : key
