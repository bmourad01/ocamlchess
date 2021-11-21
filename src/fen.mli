open Base

(** FEN string representation of the starting Chess position. *)
val start : string

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Raises [Invalid_argument] if [s] is not a valid FEN
    string. *)
val of_string_exn : string -> Board.t

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Returns [None] if [s] is not a valid FEN string. *)
val of_string : string -> Board.t option

(** [create ()] constructs the starting position. *)
val create : unit -> Board.t

(** [to_string fen] returns a string representation of [fen]. *)
val to_string : Board.t -> string
