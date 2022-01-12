(** The number of bits required to store the move information. *)
val bits : int

(** The representation of a move as an unboxed integer. The notation used is
    the same as the UCI protocol, with a source square, a destination square,
    and an optional piece promotion. *)
type t = private int [@@deriving compare, equal, hash, sexp]

include Base.Comparable.S with type t := t

(** [create src dst ~promote] creates a move from square [src] to square
    [dst], with an optional promotion [promote]. *)
val create : ?promote:Piece.kind option -> Square.t -> Square.t -> t

(** [create_with_promote src dst promote] creates a move from square [src] to
    square [dst], with an explicit promotion [promote]. *)
val create_with_promote : Square.t -> Square.t -> Piece.kind -> t

(** [src m] returns the source square of move [m]. *)
val src : t -> Square.t

(** [dst m] returns the destination square of move [m]. *)
val dst : t -> Square.t

(** [promote m] returns the piece promotion of move [m], if it exists. *)
val promote : t -> Piece.kind option

(** [decomp m] returns a triple containing the source, destination and
    promotion of move [m], in that order. *)
val decomp : t -> Square.t * Square.t * Piece.kind option

(** [to_string m] returns the UCI-compatible string notation of move [m]. *)
val to_string : t -> string

(** [of_string_exn s] parses [s] as a UCI-compatible string notation of a
    move, and raises [Invalid_argument] upon failure. *)
val of_string_exn : string -> t

(** [of_string s] parses [s] as a UCI-compatible string notation of a move,
    and returns [None] upon failure. *)
val of_string : string -> t option
