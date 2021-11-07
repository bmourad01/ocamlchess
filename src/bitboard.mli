open Base

(** Representation of the board as a 64-bit number. Each set bit represents
    an occupied square. The LSB represents the a1 square, while the MSB
    represents the h8 square. *)
type t = private Int64.t [@@deriving compare, equal, hash, sexp]

include Comparable.S with type t := t

(** [of_int64 i] creates a bitboard with the bit representation [i]. *)
val of_int64 : Int64.t -> t

(** [to_int64 b] returns the underlying bit representation of [b]. *)
val to_int64 : t -> Int64.t

(** [empty] is the empty board. *)
val empty : t

(** [full] is the fully-occupied board. *)
val full : t

(** [inter x y] is the intersection of bitboards [x] and [y]. *)
val inter : t -> t -> t

(** [x & y] is equivalent to [inter x y]. *)
val ( & ) : t -> t -> t

(** [union x y] is the union of bitboards [x] and [y]. *)
val union : t -> t -> t

(** [x + y] is equivalent to [union x y]. *)
val ( + ) : t -> t -> t

(** [compl b] is the complement of a bitboard [b]. *)
val compl : t -> t

(** [~~b] is equivalent to [compl b]. *)
val ( ~~ ) : t -> t

(** [diff x y] is equivalent to [inter x (compl y)]. *)
val diff : t -> t -> t

(** [x - y] is equivalent to [diff x y]. *)
val ( - ) : t -> t -> t

(** [count b] returns the number of occupied squares in [b]. *)
val count : t -> int

(** [$b] is equivalent to [count b]. *)
val ( $ ) : t -> int

(** [singleton sq] returns the bitboard with only the square [sq] occupied. *)
val singleton : Square.t -> t

(** [!sq] is equivalent to [singleton sq]. *)
val ( ! ) : Square.t -> t

(** [set b sq] is equivalent to [union b (singleton sq)]. *)
val set : t -> Square.t -> t

(** [b <-- sq] is equivalent to [set b sq]. *)
val ( <-- ) : t -> Square.t -> t

(** [clear b sq] is equivalent to [inter b (compl (singleton sq))] *)
val clear : t -> Square.t -> t

(** [b --> sq] is equivalent to [clear b sq]. *)
val ( --> ) : t -> Square.t -> t

(** [mem b sq] tests if the square [sq] is occupied in [b]. *)
val mem : t -> Square.t -> bool

(** [sq @ b] is equivalent to [mem b sq]. *)
val ( @ ) : Square.t -> t -> bool

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
