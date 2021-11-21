open Base

(** FEN string representation of the starting Chess position. *)
val start_string : string

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Raises [Invalid_argument] if [s] is not a valid FEN
    string. *)
val of_string_exn : string -> Position.t

(** [of_string_exn s] attempts to parse a FEN string [s] into a valid
    representation. Returns [None] if [s] is not a valid FEN string. *)
val of_string : string -> Position.t option

(** The starting position. *)
val start : Position.t

(** [to_string fen] returns a string representation of [fen]. *)
val to_string : Position.t -> string
