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

module Info : sig
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

  and score = {
    cp : float;
    mate : int;
    bound : [`lower | `upper];
  }

  and currline = {
    cpunr : int;
    moves : Move.t list;
  }

  val to_string : t -> string
end

module Option : sig
  module Spin : sig
    type t = {
      default : int;
      min : int;
      max : int
    }

    val to_string : t -> string
  end

  module Check : sig
    type t = {
      default : bool
    }

    val to_string : t -> string
  end

  module Combo : sig
    type t = {
      default : string;
      var : string list
    }

    val to_string : t -> string
  end

  module String : sig
    type t = {
      default : string
    }

    val to_string : t -> string
  end

  type t =
    | Hash of Spin.t
    | NalimovPath of String.t
    | NalimovCache of Spin.t
    | Ponder of Check.t
    | OwnBook of Check.t
    | MultiPV of Spin.t
    | UCI_ShowCurrLine of Check.t
    | UCI_ShowRefutations of Check.t
    | UCI_LimitStrength of Check.t
    | UCI_Elo of Spin.t
    | UCI_AnalyseMode of Check.t
    | UCI_Opponent of String.t
    | UCI_EngineAbout of String.t
    | UCI_ShredderbasesPath of String.t
    | UCI_SetPositionValue of String.t

  val to_string : t -> string
end

module Recv : sig
  type t =
    | Uci
    | Debug of [`on | `off]
    | Isready
    | Setoption of Option.t
    | Register of [`later | `name of string | `code of string]
    | Ucinewgame
    | Position of [`fen of string | `startpos] * Move.t list
    | Go of Go.t
    | Stop
    | Ponderhit
    | Quit

  val to_string : t -> string
end

module Send : sig
  type t =
    | Id of [`name of string | `author of string]
    | Uciok
    | Readyok
    | Bestmove of bestmove
    | Copyprotection of [`checking | `ok | `error]
    | Registration of [`checking | `ok | `error]
    | Info of Info.t
    | Option of Option.t

  and bestmove = {
    move : Move.t;
    ponder : Move.t option
  }

  val to_string : t -> string
end

type t = Recv of Recv.t | Send of Send.t

val to_string : t -> string
