(** Number of bits needed to store the square. *)
val bits : int

(** Number of squares on the board. *)
val count : int

(** Represents a square on the board as an unboxed integer. Must be within
    the range [\[0, 63\]]. *)
type t = private int [@@deriving compare, equal, hash, sexp]

include Base.Comparable.S with type t := t

(** [of_int_exn i] creates a square from an integer [i]. Raises
    [Invalid_argument] if the integer is not within the specified range. *)
val of_int_exn : int -> t

(** [of_int_unsafe i] creates a square from an integer [i]. Does not perform
    any sanity checks on [i]. *)
val of_int_unsafe : int -> t

(** [of_int i] creates a square from an integer [i]. Returns [None] if the
    integer is not within the specified range. *)
val of_int : int -> t option

(** [create_exn ~rank ~file] creates a square from the rank and file indicies
    [rank] and [file], respectively. Both represent indices, and so they must
    fall within the range [\[0, 7\]], otherwise [Invalid_argument] is raised. *)
val create_exn : rank:int -> file:int -> t

(** [create_unsafe ~rank ~file] creates a square from the rank and file
    indicies [rank] and [file], respectively. Both represent indices, and so
    they must fall within the range [\[0, 7\]]. No sanity checks are performed,
    so invalid arguments may result in undefined behavior. *)
val create_unsafe : rank:int -> file:int -> t

(** [create_exn ~rank ~file] creates a square from the rank and
    file indicies [rank] and [file], respectively. Both represent indices,
    and so they must fall within the range [\[0, 7\]], otherwise [None] is
    returned. *)
val create : rank:int -> file:int -> t option

(** [with_rank_exn sq rank] sets the rank of [sq] to [rank]. [rank] must fall
    within the range [\[0, 7\]], otherwise [Invalid_argument] is raised. *)
val with_rank_exn : t -> int -> t

(** [with_file_exn sq file] sets the file of [sq] to [file]. [file] must fall
    within the range [\[0, 7\]], otherwise [Invalid_argument] is raised. *)
val with_file_exn : t -> int -> t

(** [with_rank_unsafe sq rank] sets the rank of [sq] to [rank]. No sanity
    checks are performed, so invalid arguments may result in undefined
    behavior. *)
val with_rank_unsafe : t -> int -> t

(** [with_file_unsafe sq file] sets the file of [sq] to [file]. No sanity
    checks are performed, so invalid arguments may result in undefined
    behavior. *)
val with_file_unsafe : t -> int -> t

(** [with_rank sq rank] sets the rank of [sq] to [rank]. [rank] must fall
    within the range [\[0, 7\]], otherwise [None] is returned. *)
val with_rank : t -> int -> t option

(** [with_file sq file] sets the file of [sq] to [file]. [file] must fall
    within the range [\[0, 7\]], otherwise [None] is returned. *)
val with_file : t -> int -> t option

(** [to_int sq] returns the integer representation of [sq]. *)
val to_int : t -> int

(** [rank sq] returns the rank index for [sq], which is within the range
    [\[0, 7\]]. *)
val rank : t -> int

(** [rank_char sq] returns the character representation of the rank for [sq],
    which is within the range ['1'..'8']. *)
val rank_char : t -> char

(** [file sq] returns the file index for [sq], which is within the range
    [\[0, 7\]]. *)
val file : t -> int

(** [rank_char sq] returns the character representation of the file for [sq],
    which is within the range ['a'..'h']. *)
val file_char : t -> char

(** [decomp sq] returns a pair containing the rank and file of [sq],
    in that order. *)
val decomp : t -> (int * int)

(** [of_string_exn s] parses [s] to the underlying representation of a
    square. [s] must obey the regular language [\[a-h\]\[1-8\]], otherwise
    [Invalid_argument] is raised. *)
val of_string_exn : string -> t

(** [of_string_exn s] parses [s] to the underlying representation of a
    square. [s] must obey the regular language [\[a-h\]\[1-8\]], otherwise
    [None] is returned. *)
val of_string : string -> t option

(** [to_string sq] returns the string representation of [sq], which obeys the
    regular language [\[a-h\]\[1-8\]]. *)
val to_string : t -> string

(** The a1 square. *)
val a1 : t

(** The a2 square. *)
val a2 : t

(** The a3 square. *)
val a3 : t

(** The a4 square. *)
val a4 : t

(** The a5 square. *)
val a5 : t

(** The a6 square. *)
val a6 : t

(** The a7 square. *)
val a7 : t

(** The a8 square. *)
val a8 : t

(** The b1 square. *)
val b1 : t

(** The b2 square. *)
val b2 : t

(** The b3 square. *)
val b3 : t

(** The b4 square. *)
val b4 : t

(** The b5 square. *)
val b5 : t

(** The b6 square. *)
val b6 : t

(** The b7 square. *)
val b7 : t

(** The b8 square. *)
val b8 : t

(** The c1 square. *)
val c1 : t

(** The c2 square. *)
val c2 : t

(** The c3 square. *)
val c3 : t

(** The c4 square. *)
val c4 : t

(** The c5 square. *)
val c5 : t

(** The c6 square. *)
val c6 : t

(** The c7 square. *)
val c7 : t

(** The c8 square. *)
val c8 : t

(** The d1 square. *)
val d1 : t

(** The d2 square. *)
val d2 : t

(** The d3 square. *)
val d3 : t

(** The d4 square. *)
val d4 : t

(** The d5 square. *)
val d5 : t

(** The d6 square. *)
val d6 : t

(** The d7 square. *)
val d7 : t

(** The d8 square. *)
val d8 : t

(** The e1 square. *)
val e1 : t

(** The e2 square. *)
val e2 : t

(** The e3 square. *)
val e3 : t

(** The e4 square. *)
val e4 : t

(** The e5 square. *)
val e5 : t

(** The e6 square. *)
val e6 : t

(** The e7 square. *)
val e7 : t

(** The e8 square. *)
val e8 : t

(** The f1 square. *)
val f1 : t

(** The f2 square. *)
val f2 : t

(** The f3 square. *)
val f3 : t

(** The f4 square. *)
val f4 : t

(** The f5 square. *)
val f5 : t

(** The f6 square. *)
val f6 : t

(** The f7 square. *)
val f7 : t

(** The f8 square. *)
val f8 : t

(** The g1 square. *)
val g1 : t

(** The g2 square. *)
val g2 : t

(** The g3 square. *)
val g3 : t

(** The g4 square. *)
val g4 : t

(** The g5 square. *)
val g5 : t

(** The g6 square. *)
val g6 : t

(** The g7 square. *)
val g7 : t

(** The g8 square. *)
val g8 : t

(** The h1 square. *)
val h1 : t

(** The h2 square. *)
val h2 : t

(** The h3 square. *)
val h3 : t

(** The h4 square. *)
val h4 : t

(** The h5 square. *)
val h5 : t

(** The h6 square. *)
val h6 : t

(** The h7 square. *)
val h7 : t

(** The h8 square. *)
val h8 : t

(** Integer representations of the ranks. *)
module Rank : sig
  (** The first rank. *)
  val one : int

  (** The second rank. *)
  val two : int

  (** The third rank. *)
  val three : int

  (** The fourth rank. *)
  val four : int

  (** The fifth rank. *)
  val five : int

  (** The sixth rank. *)
  val six : int

  (** The seventh rank. *)
  val seven : int

  (** The eighth rank. *)
  val eight : int

  (** Number of ranks. *)
  val count : int

  (** The mask for valid ranks. *)
  val mask : int

  (** The mask for invalid ranks. *)
  val nmask : int
end

(** Integer represenations of the files. *)
module File : sig
  (** The a file. *)
  val a : int

  (** The b file. *)
  val b : int

  (** The c file. *)
  val c : int

  (** The d file. *)
  val d : int

  (** The e file. *)
  val e : int

  (** The f file. *)
  val f : int

  (** The g file. *)
  val g : int

  (** The h file. *)
  val h : int

  (** Number of files. *)
  val count : int

  (** The mask for valid files. *)
  val mask : int

  (** The mask for invalid files. *)
  val nmask : int
end

(** The first square that is visited (in integer order). *)
val first : int

(** The last square that is visited (in integer order). *)
val last : int 

(** [chebyshev sq1 sq2] returns the Chebyshev distance between [sq1] and [sq2].
    This is equivalent to the total number of moves needed to move a king from
    [sq1] to [sq2], or vice versa. *)
val chebyshev : t -> t -> int
