(** This module contains utilities for Zobrist hashing. The random numbers
    are the same as those used in the Polyglot chess adapter. *)

type key = int64 [@@deriving equal, compare, sexp]

(** A hash table with a provided replacement strategy. *)
module Table : sig
  (** The slot with entries of type ['a]. *)
  type 'a slot

  module Slot : sig
    type 'a t = 'a slot

    (** The entry assocated with the slot. *)
    val entry : 'a t -> 'a

    (** The key associated with the slot. *)
    val key : 'a t -> key

    (** The age associated with the slot. *)
    val age : 'a t -> int
  end

  (** The replacement strategy, where [prev] is the existing slot. *)
  type 'a replace = prev:'a slot -> 'a -> key -> bool

  (** The aging strategy for slots. If  the result is [true], then
      the age of the slot is is incremented. Otherwise, the slot is
      evicted.

      The "age" of the slot can be seen as the number of ply since
      the slot was added to the table.
  *)
  type 'a age = 'a slot -> bool

  (** The hash table. *)
  type 'a t

  exception Not_found

  (** [create ~capacity ~replace] creates the table.

      [capacity] is the maximum number of slots allowed in the table.
      It is increased to the smallest power of two such that it is greater
      than or equal to [capacity].

      [replace] is the replacement strategy for the table.
  *)
  val create : capacity:int -> replace:'a replace -> age:'a age -> 'a t

  (** [clear t] evicts all slots from the table. *)
  val clear : 'a t -> unit

  (** [get t key] attempts to find the slot associated with [key], and
      returns [None] if this slot is unoccupied. *)
  val get : 'a t -> key -> 'a slot option

  (** [get_exn t key] attempts to find the slot associated with [key], and
      raises [Not_found] if this slot is unoccupied. *)
  val get_exn : 'a t -> key -> 'a slot

  (** Same as [get t key], but applies [Slot.entry] to the result (if it
      exists). *)
  val get_entry : 'a t -> key -> 'a option

  (** Same as [get_exn t key], but applies [Slot.entry] to the result (if
      it exists). *)
  val get_entry_exn : 'a t -> key -> 'a

  (** [set t key entry] will attempt to set the slot associated with [key]
      to [entry]. If the slot is already occupied, then the provided
      replacement strategy will determine if the slot gets updated. *)
  val set : 'a t -> key -> 'a -> unit

  (** [age t] applies the provided aging strategy to all slots in the
      table. *)
  val age : 'a t -> unit
end

type 'a table = 'a Table.t

(** [piece c k sq] returns the random number associated with the piece of
    color [c] and kind [k] at square [sq]. *)
val piece : Piece.color -> Piece.kind -> Square.t -> key

(** [en_passant sq] returns the random number associated with the en passant
    square [sq]. *)
val en_passant : Square.t -> key

(** [castle c s] returns the random number associated with the castling rights
    for color [c] and side [s]. *)
val castle : Piece.color -> Castling_rights.side -> key

(** [white_to_move] is the key associated with whether the active color is
    white. *)
val white_to_move : key
