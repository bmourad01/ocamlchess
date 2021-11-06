open Base

type color = White | Black [@@deriving compare, equal, hash, sexp]

module Color : sig
  type t = color [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t
end

type kind = Pawn | Knight | Bishop | Rook | Queen | King
[@@deriving compare, equal, hash, sexp]

module Kind : sig
  type t = kind [@@deriving compare, equal, hash, sexp]

  include Comparable.S with type t := t
end

type t = private int [@@deriving compare, equal, hash, sexp]

val white_pawn : t

val white_knight : t

val white_bishop : t

val white_rook : t

val white_queen : t

val white_king : t

val black_pawn : t

val black_knight : t

val black_bishop : t

val black_rook : t

val black_queen : t

val black_king : t

val color : t -> color

val kind : t -> kind

val create : color -> kind -> t

val is_white : t -> bool

val is_black : t -> bool

val is_pawn : t -> bool

val is_knight : t -> bool

val is_bishop : t -> bool

val is_rook : t -> bool

val is_queen : t -> bool

val is_king : t -> bool

val is_white_pawn : t -> bool

val is_white_knight : t -> bool

val is_white_bishop : t -> bool

val is_white_rook : t -> bool

val is_white_queen : t -> bool

val is_white_king : t -> bool

val is_black_pawn : t -> bool

val is_black_knight : t -> bool

val is_black_bishop : t -> bool

val is_black_rook : t -> bool

val is_black_queen : t -> bool

val is_black_king : t -> bool

val of_int_exn : int -> t

val of_int : int -> t option

val to_int : t -> int

val of_fen_exn : char -> t

val of_fen : char -> t option

val to_fen : t -> char
