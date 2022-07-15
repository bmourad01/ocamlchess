(** The flagship player. *)

(** Search limits. *)
val limits : Chess.Search.limits option ref

(** Opening book. *)
val book : Chess.Book.t option ref

(** The player name. *)
val name : string

(* (\** Create the transposition table. *\) *)
(* val create_tt : unit -> Chess.Search.tt *)

(** Create the player. *)
val create : unit -> Player.e
