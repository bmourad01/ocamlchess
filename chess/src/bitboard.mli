open Base

(** Representation of the board as a 64-bit number. Each set bit represents
    an occupied square. The LSB represents the a1 square, while the MSB
    represents the h8 square. *)
type t = private int64 [@@deriving compare, equal, hash, sexp]

include Comparable.S with type t := t

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

(** [diff x y] is equivalent to [inter x (compl y)]. *)
val diff : t -> t -> t

(** [xor x y] is the exclusive-OR of boards [x] and [y]. *)
val xor : t -> t -> t

(** [count b] returns the number of occupied squares in [b]. *)
val count : t -> int

(** [singleton sq] returns the bitboard with only the square [sq] occupied. *)
val singleton : Square.t -> t

(** [set b sq] is equivalent to [union b (singleton sq)]. *)
val set : t -> Square.t -> t

(** [clear b sq] is equivalent to [diff b (singleton sq)] *)
val clear : t -> Square.t -> t

(** [mem b sq] tests if the square [sq] is occupied in [b]. *)
val mem : t -> Square.t -> bool

(** [fold b ~init ~f ~rev] accumulates a result over each occupied square in
    [b]. If [rev] is [true], then iteration starts from square h8, otherwise
    from a1. *)
val fold : ?rev:bool -> t -> init:'a -> f:('a -> Square.t -> 'a) -> 'a

(** [fold_until b ~init ~f ~finish ~rev] is a short-circuiting version of
    [fold]. If [f] returns [Stop x], the result of the computation is [x]. If
    [f] returns [Continue y], then the computation continues with [y] as the
    new accumulated value. If [f] never returns [Stop _], then the final
    result is computed by [finish]. If [rev] is [true], then iteration starts
    from square h8, otherwise from a1. *)
val fold_until :
  ?rev:bool
  -> t
  -> init:'a
  -> f:('a -> Square.t -> ('a, 'b) Continue_or_stop.t)
  -> finish:('a -> 'b)
  -> 'b

(** [iter b ~f ~rev] applies [f] to each occupied square in [b]. If [rev] is
    [true], then iteration starts from square h8, otherwise from a1. *)
val iter : ?rev:bool -> t -> f:(Square.t -> unit) -> unit

(** [iter_until b ~f ~rev] applies [f] to each occupied square in [b] until
    its result is [true]. If [rev] is [true], then iteration starts from
    square h8, otherwise from a1. *)
val iter_until : ?rev:bool -> t -> f:(Square.t -> bool) -> unit

(** [filter b ~f] applies [f] to each occupied square [sq] in [b]. If [f sq]
    returns [false], then [sq] is cleared in the resulting bitboard.
    Otherwise, it is kept. *)
val filter : t -> f:(Square.t -> bool) -> t

(** [find b ~f ~rev] returns the first square [sq] of [b] which satisfies
    [f sq], if it exists. If [rev] is [true], then iteration starts from
    square h8, otherwise from a1. *)
val find : ?rev:bool -> t -> f:(Square.t -> bool) -> Square.t option

(** [first_set_exn b] returns the first square [sq] of [b], if it exists.
    Otherwise, [Invalid_argument] is raised. If [rev] is [true], then iteration
    starts from square h8, otherwise from a1. *)
val first_set_exn : ?rev:bool -> t -> Square.t

(** [first_set b] returns the first square [sq] of [b], if it exists.
    Otherwise, [None] is returned. If [rev] is [true], then iteration starts
    from square h8, otherwise from a1. *)
val first_set : ?rev:bool -> t -> Square.t option

module Syntax : sig
  (** [x & y] is equivalent to [inter x y]. *)
  val (&) : t -> t -> t

  (** [x + y] is equivalent to [union x y]. *)
  val (+) : t -> t -> t

  (** [x - y] is equivalent to [diff x y]. *)
  val (-) : t -> t -> t

  (** [x ^ y] is equivalent to [xor x y]. *)
  val (^) : t -> t -> t

  (** [~~b] is equivalent to [compl b]. *)
  val (~~) : t -> t

  (** [!!sq] is equivalent to [singleton sq]. *)
  val (!!) : Square.t -> t

  (** [b ++ sq] is equivalent to [set b sq]. *)
  val (++) : t -> Square.t -> t

  (** [b -- sq] is equivalent to [clear b sq]. *)
  val (--) : t -> Square.t -> t

  (** [sq @ b] is equivalent to [mem b sq]. *)
  val (@) : Square.t -> t -> bool
end

include module type of Syntax
