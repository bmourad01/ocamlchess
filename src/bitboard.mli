open Base

(** Representation of the board as a 64-bit number. Each set bit represents
    an occupied square. The MSB represents the a1 square, while the LSB
    represents the h8 square. *)
type t = private Int64.t [@@deriving compare, equal, hash, sexp]

include Comparable.S with type t := t

(** [empty] is the empty board. *)
val empty : t

(** [full] is the fully-occupied board. *)
val full : t

(** [inter x y] is the intersection of bitboards [x] and [y]. *)
val inter : t -> t -> t

(** [union x y] is the union of bitboards [x] and [y]. *)
val union : t -> t -> t

(** [compl b] is the complement of a bitboard [b]. *)
val compl : t -> t

(** [singleton sq] returns the empty bitboard, with square [sq] occupied. *)
val singleton : Square.t -> t

(** [set b sq] is equivalent to [union b (singleton sq)]. *)
val set : t -> Square.t -> t

(** [clear b sq] is equivalent to [inter b (compl (singleton sq))] *)
val clear : t -> Square.t -> t

(** [mem b sq] tests if the square [sq] is occupied in [b]. *)
val mem : t -> Square.t -> bool

(** [fold b ~init ~f ~rev] accumulates a result over each occupied square in
    [b]. If [rev] is [true], then iteration starts from square h8, otherwise
    from a1. *)
val fold : ?rev:bool -> t -> init:'a -> f:('a -> Square.t -> 'a) -> 'a

(** [iter b ~f ~rev] applies [f] to each occupied square in [b]. If [rev] is
    [true], then iteration starts from square h8, otherwise from a1. *)
val iter : ?rev:bool -> t -> f:(Square.t -> unit) -> unit

(** [count b] returns the number of occupied squares in [b]. *)
val count : t -> int

(** [of_int64 i] creates a bitboard with the bit representation [i]. *)
val of_int64 : Int64.t -> t

(** [to_int64 b] returns the underlying bit representation of [b]. *)
val to_int64 : t -> Int64.t
