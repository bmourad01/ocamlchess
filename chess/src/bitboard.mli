(** This module implements support for the bitboard representation. *)

(** Representation of the board as a 64-bit number. Each set bit represents
    an occupied square. The LSB represents the a1 square, while the MSB
    represents the h8 square. Files are ascended first, then ranks. *)
type t = private int64 [@@deriving compare, equal, hash, sexp]

include Base.Comparable.S with type t := t

(** [of_int64 i] creates a bitboard with the bit representation [i]. *)
val of_int64 : int64 -> t

(** [to_int64 b] returns the underlying bit representation of [b]. *)
val to_int64 : t -> int64

(** [empty] is the empty board. *)
val empty : t

(** [full] is the fully-occupied board. *)
val full : t

(** [rank_1] is the first rank. *)
val rank_1 : t

(** [rank_2] is the second rank. *)
val rank_2 : t

(** [rank_3] is the third rank. *)
val rank_3 : t

(** [rank_4] is the fourth rank. *)
val rank_4 : t

(** [rank_5] is the fifth rank. *)
val rank_5 : t

(** [rank_6] is the sixth rank. *)
val rank_6 : t

(** [rank_7] is the seventh rank. *)
val rank_7 : t

(** [rank_8] is the eighth rank. *)
val rank_8 : t

(** [file_a] is the a file. *)
val file_a : t

(** [file_b] is the b file. *)
val file_b : t

(** [file_c] is the c file. *)
val file_c : t

(** [file_d] is the d file. *)
val file_d : t

(** [file_e] is the e file. *)
val file_e : t

(** [file_f] is the f file. *)
val file_f : t

(** [file_g] is the g file. *)
val file_g : t

(** [file_h] is the h file. *)
val file_h : t

(** [edges] is the union of [file_a], [file_h], [rank_1], and [rank_8]. *)
val edges : t

(** [center] is the four center squares: d4, e4, d5, and e5. *)
val center : t

(** [black] is the set of black squares. *)
val black : t

(** [white] is the set of white squares. *)
val white : t

(** [rank_exn i] the rank at index [i] if [i \in \[0,7\]]. Otherwise, an
    exception is raised. *)
val rank_exn : int -> t

(** [file_exn i] the file at index [i] if [i \in \[0,7\]]. Otherwise, an
    exception is raised. *)
val file_exn : int -> t

(** [rank i] the rank at index [i] if [i \in \[0,7\]]. Otherwise, [None] is
    returned *)
val rank : int -> t option

(** [file i] the file at index [i] if [i \in \[0,7\]]. Otherwise, [None] is
    returned *)
val file : int -> t option

(** [inter x y] is the intersection of bitboards [x] and [y]. *)
val inter : t -> t -> t

(** [union x y] is the union of bitboards [x] and [y]. *)
val union : t -> t -> t

(** [compl b] is the complement of a bitboard [b]. *)
val compl : t -> t

(** [minus x y] is equivalent to [inter x (compl y)]. *)
val minus : t -> t -> t

(** [diff x y] is the difference between (exclusive-OR of) boards [x] and
    [y]. *)
val diff : t -> t -> t

(** [shl x n] shifts [x] left by [n] bits. *)
val shl : t -> int -> t

(** [shr x n] shifts [x] right by [n] bits. *)
val shr : t -> int -> t

(** [count b] returns the number of occupied squares in [b]. *)
val count : t -> int

(** [singleton sq] returns the bitboard with only the square [sq] occupied. *)
val singleton : Square.t -> t

(** [set b sq] is equivalent to [union b (singleton sq)]. *)
val set : t -> Square.t -> t

(** [clear b sq] is equivalent to [minus b (singleton sq)] *)
val clear : t -> Square.t -> t

(** [mem b sq] tests if the square [sq] is occupied in [b]. *)
val mem : t -> Square.t -> bool

(** [fold b ~init ~f] accumulates a result over each occupied square in
    [b], starting from square a1. *)
val fold : t -> init:'a -> f:('a -> Square.t -> 'a) -> 'a

(** [fold_rev b ~init ~f] accumulates a result over each occupied square in
    [b], starting from square h8. *)
val fold_rev : t -> init:'a -> f:('a -> Square.t -> 'a) -> 'a

(** [fold_until b ~init ~f ~finish] is a short-circuiting version of [fold].
    If [f] returns [Stop x], the result of the computation is [x]. If
    [f] returns [Continue y], then the computation continues with [y] as the
    new accumulated value. If [f] never returns [Stop _], then the final
    result is computed by [finish]. *)
val fold_until :
  t ->
  init:'a ->
  f:('a -> Square.t -> ('a, 'b) Base.Continue_or_stop.t) ->
  finish:('a -> 'b) ->
  'b

(** [fold_until_rev b ~init ~f ~finish] is a short-circuiting version of
    [fold_rev]. If [f] returns [Stop x], the result of the computation is [x].
    If [f] returns [Continue y], then the computation continues with [y] as the
    new accumulated value. If [f] never returns [Stop _], then the final
    result is computed by [finish]. *)
val fold_until_rev :
  t ->
  init:'a ->
  f:('a -> Square.t -> ('a, 'b) Base.Continue_or_stop.t) ->
  finish:('a -> 'b) ->
  'b

(** [iter b ~f] applies [f] to each occupied square in [b], starting from
    a1. *)
val iter : t -> f:(Square.t -> unit) -> unit

(** [iter_rev b ~f] applies [f] to each occupied square in [b], starting from
    h8. *)
val iter_rev : t -> f:(Square.t -> unit) -> unit

(** [iter_until b ~f] applies [f] to each occupied square in [b], starting from
    a1, until its result is [true]. *)
val iter_until : t -> f:(Square.t -> bool) -> unit

(** [iter_until_rev b ~f] applies [f] to each occupied square in [b], starting
    from h8, until its result is [true]. *)
val iter_until_rev : t -> f:(Square.t -> bool) -> unit

(** [filter b ~f] applies [f] to each occupied square [sq] in [b]. If [f sq]
    returns [false], then [sq] is cleared in the resulting bitboard.
    Otherwise, it is kept. *)
val filter : t -> f:(Square.t -> bool) -> t

(** [find b ~f] returns the first square [sq] of [b], starting from a1, which
    satisfies [f sq], if it exists. *)
val find : t -> f:(Square.t -> bool) -> Square.t option

(** [find_rev b ~f] returns the first square [sq] of [b], starting from h8,
    which satisfies [f sq], if it exists. *)
val find_rev : t -> f:(Square.t -> bool) -> Square.t option

(** [to_list b] returns the list of squares set in [b], in ascending order. *)
val to_list : t -> Square.t list

(** [to_list_rev b] returns the list of squares set in [b], in descending
    order. *)
val to_list_rev : t -> Square.t list

(** [first_set_exn b] returns the first square [sq] of [b], starting from a1,
    if it exists. Otherwise, [Invalid_argument] is raised. *)
val first_set_exn : t -> Square.t

(** [first_set_rev_exn b] returns the first square [sq] of [b], starting from
    h8, if it exists. Otherwise, [Invalid_argument] is raised. *)
val first_set_rev_exn : t -> Square.t

(** [first_set b] returns the first square [sq] of [b], starting from a1, if it
    exists. Otherwise, [None] is returned. *)
val first_set : t -> Square.t option

(** [first_set_rev b] returns the first square [sq] of [b], starting from h8,
    if it exists. Otherwise, [None] is returned. *)
val first_set_rev : t -> Square.t option

module Syntax : sig
  (** [x & y] is equivalent to [inter x y]. *)
  val (&) : t -> t -> t

  (** [x + y] is equivalent to [union x y]. *)
  val (+) : t -> t -> t

  (** [x - y] is equivalent to [minus x y]. *)
  val (-) : t -> t -> t

  (** [x ^ y] is equivalent to [diff x y]. *)
  val (^) : t -> t -> t

  (** [~~b] is equivalent to [compl b]. *)
  val (~~) : t -> t

  (** [!!sq] is equivalent to [singleton sq]. *)
  val (!!) : Square.t -> t

  (** [b ++ sq] is equivalent to [set b sq]. *)
  val (++) : t -> Square.t -> t

  (** [b -- sq] is equivalent to [clear b sq]. *)
  val (--) : t -> Square.t -> t

  (** [x << n] is equivalent to [shl x n]. *)
  val (<<) : t -> int -> t

  (** [x >> n] is equivalent to [shr x n]. *)
  val (>>) : t -> int -> t

  (** [sq @ b] is equivalent to [mem b sq]. *)
  val (@) : Square.t -> t -> bool
end

include module type of Syntax
