(** This module implements support for the PGN (Portable Game Notation)
    format. *)

(** Draws that must be declared by a player. *)
type declared_draw = [
  | `Mutual_agreement
  | `Threefold_repetition
  | `Fifty_move_rule
]

(** Draws that are automatic. *)
type automatic_draw = [
  | `Stalemate
  | `Insufficient_material
  | `Fivefold_repetition
  | `Seventy_five_move_rule
]

(** The kind of draw for the game. *)
type draw = [declared_draw | automatic_draw] [@@deriving compare, equal, sexp]

module Draw : sig
  (** The kind of draw for the game. *)
  type t = draw [@@deriving compare, equal, sexp]

  (** Returns [true] if the draw was declared by a player. *)
  val is_declared : t -> bool

  (** Returns [true] if the draw was automatic. *)
  val is_automatic : t -> bool

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

(** [add_move game legal ~resigned ~declared_draw] updates [game] with a new
    move [legal]. [resigned] indicates that a player resigned after the move was
    made. [declared_draw] optionally denotes that a player declared a draw after
    the move was made. Raises [Failure] when [game] has already ended. Raises
    [Invalid_argument] when [legal] has a different parent position than the
    move than the most recent move made in [game]. *)
val add_move :
  ?resigned:Piece.color option ->
  ?declared_draw:declared_draw option ->
  t ->
  Position.legal ->
  t

(** Returns a PGN representation of the game. *)
val to_string : t -> string
