open Base

type t = private int [@@deriving compare, equal, hash, sexp]

include Comparable.S with type t := t

val of_int_exn : int -> t

val of_int : int -> t option

val of_rank_and_file_idx_exn : rank:int -> file:int -> t

val of_rank_and_file_idx : rank:int -> file:int -> t option

val to_int : t -> int

val rank_idx : t -> int

val rank_char : t -> char

val file_idx : t -> int

val file_char : t -> char

val of_string_exn : string -> t

val of_string : string -> t option

val to_string : t -> string

val a1 : t

val a2 : t

val a3 : t

val a4 : t

val a5 : t

val a6 : t

val a7 : t

val a8 : t

val b1 : t

val b2 : t

val b3 : t

val b4 : t

val b5 : t

val b6 : t

val b7 : t

val b8 : t

val c1 : t

val c2 : t

val c3 : t

val c4 : t

val c5 : t

val c6 : t

val c7 : t

val c8 : t

val d1 : t

val d2 : t

val d3 : t

val d4 : t

val d5 : t

val d6 : t

val d7 : t

val d8 : t

val e1 : t

val e2 : t

val e3 : t

val e4 : t

val e5 : t

val e6 : t

val e7 : t

val e8 : t

val f1 : t

val f2 : t

val f3 : t

val f4 : t

val f5 : t

val f6 : t

val f7 : t

val f8 : t

val g1 : t

val g2 : t

val g3 : t

val g4 : t

val g5 : t

val g6 : t

val g7 : t

val g8 : t

val h1 : t

val h2 : t

val h3 : t

val h4 : t

val h5 : t

val h6 : t

val h7 : t

val h8 : t
