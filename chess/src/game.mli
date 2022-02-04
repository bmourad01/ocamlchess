(** This module implements support for the PGN (Portable Game Notation)
    format. *)

(** The kind of draw for the game. *)
type draw =
  | Stalemate
  | Insufficient_material
  | Mutual_agreement
  | Threefold_repetition
  | Fivefold_repetition
  | Fifty_move_rule
[@@deriving compare, equal, sexp]

module Draw : sig
  (** The kind of draw for the game. *)
  type t = draw [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t
end

(** The result of the game. [Ongoing] means that the game has not ended yet. *)
type result =
  | Checkmate of Piece.color
  | Resigned of Piece.color
  | Draw of draw
  | Ongoing
[@@deriving compare, equal, sexp]

module Result : sig
  (** The result of the game. *)
  type t = result [@@deriving compare, equal, sexp]

  include Base.Comparable.S with type t := t
end

(** The game that was (or is being) played. *)
type t [@@deriving compare, equal, sexp]

include Base.Comparable.S with type t := t

(** The name of the event, if any. *)
val event : t -> string option

(** The location of the event, if any. *)
val site : t -> string option

(** The date the game was played, if any. *)
val date : t -> Core_kernel.Date.t option

(** The round of the event (such as in a tournament), if any. *)
val round : t -> int option 

(** The name of the player with the white pieces, if any. *)
val white : t -> string option

(** The name of the player with the black pieces, if any. *)
val black : t -> string option

(** The result of the game. *)
val result : t -> result

(** The starting position. *)
val start : t -> Position.t

(** The list of moves that were played. The order of moves is reversed,
    with the last move being played at the beginning of the list. *)
val moves : t -> Position.legal list

(** Returns the current position of the game. *)
val position : t -> Position.t

(** Returns [true] if the game is over. *)
val is_over : t -> bool

(** Creates a new game. *)
val create :
  ?event:string option ->
  ?site:string option ->
  ?date:Core_kernel.Date.t option ->
  ?round:int option ->
  ?white:string option ->
  ?black:string option ->
  ?start:Position.t ->
  unit ->
  t

(** [add_move game legal result] updates [game] with a new move [legal] and
    a result [result]. Raises [Failure] when [game] has already reached a
    conclusion. Raises [Invalid_argument] when [legal] has a different
    parent position than the move than the most recent move made in [game]. *)
val add_move : t -> Position.legal -> result -> t

(** Returns a PGN representation of the game. *)
val to_string : t -> string
