(** This module implements data structures and interfaces for constructing and
    reading UCI commands. *)

(** This submodule implements the datatypes for commands that are received by
    the engine from the GUI. *)
module Recv : sig
  (** Sent by the user when they want to change the internal parameters of the
      engine. *)
  module Setoption : sig
    type t = {
      name : string;
      value : string option;
    }

    val to_string : t -> string
  end

  (** Start calculating on the current position. *)
  module Go : sig
    type t =
      | Searchmoves of Move.t list
      | Ponder
      | Wtime of int
      | Btime of int
      | Winc of int
      | Binc of int
      | Movestogo of int
      | Depth of int
      | Nodes of int
      | Mate of int
      | Movetime of int
      | Infinite

    val to_string : t -> string
  end

  (** The commands that the engine may receive. *)
  type t =
    | Uci
    | Debug of [`on | `off]
    | Isready
    | Setoption of Setoption.t
    | Register of [`later | `namecode of string * string]
    | Ucinewgame
    | Position of [`fen of string | `startpos] * Move.t list
    | Go of Go.t
    | Stop
    | Ponderhit
    | Quit

  (** Textual representation of the command. *)
  val to_string : t -> string
end

(** This submodule implements the datatypes for commands that are sent to the
    GUI by the engine. *)
module Send : sig
  (** Tells the GUI which parameters can be changed in the engine. *)
  module Option : sig
    module Type : sig
      type spin = {
        default : int;
        min : int;
        max : int;
      }

      type combo = {
        default : string;
        var : string list;
      }

      type t =
        | Spin of spin
        | Check of bool
        | Combo of combo
        | String of string
        | Button

      val to_string : t -> string
    end

    type t = {
      name : string;
      typ : Type.t;
    }

    val to_string : t -> string
  end

  (** The best move of the current position, returned by the engine when the
      search has completed. *)
  module Bestmove : sig
    type t = {
      move : Move.t;
      ponder : Move.t option;
    }

    val to_string : t -> string
  end

  (** Information that the engine sends to the GUI about the search. *)
  module Info : sig
    type score = {
      cp : float;
      mate : int;
      bound : [`lower | `upper];
    }

    type currline = {
      cpunr : int;
      moves : Move.t list;
    }

    type t =
      | Depth of int
      | Seldepth of int
      | Time of int
      | Nodes of int
      | Pv of Move.t list
      | Multipv of int
      | Score of score
      | Currmove of Move.t
      | Currmovenumber of int
      | Hashfull of int
      | Nps of int
      | Tbhits of int
      | Sbhits of int
      | Cpuload of int
      | String of string
      | Refutation of Move.t list
      | Currline of currline

    val to_string : t -> string
  end

  (** The commands that the engine may send. *)
  type t =
    | Id of [`name of string | `author of string]
    | Uciok
    | Readyok
    | Bestmove of Bestmove.t
    | Copyprotection of [`checking | `ok | `error]
    | Registration of [`checking | `ok | `error]
    | Info of Info.t list
    | Option of Option.t

  (** Textual representation of the command. *)
  val to_string : t -> string
end

(** A UCI command, partitioned into whether the command is sent to the engine
    or from the engine. *)
type t = Recv of Recv.t | Send of Send.t

(** Textual representation of the command. *)
val to_string : t -> string
