open Core_kernel

let string_of_moves moves =
  List.map moves ~f:Move.to_string |> String.concat ~sep:" "

let string_of_string = function
  | "" -> "<empty>"
  | s -> s

module Recv = struct
  module Option = struct
    type t = {
      name : string;
      value : string option;
    }

    let to_string = function
      | {name; value = None} -> name
      | {name; value = Some value} -> sprintf "%s value %s" name value
  end

  module Go = struct
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

    let to_string = function
      | Searchmoves moves -> "searchmoves " ^ string_of_moves moves
      | Ponder -> "ponder"
      | Wtime t -> sprintf "wtime %d" t
      | Btime t -> sprintf "btime %d" t
      | Winc i -> sprintf "winc %d" i
      | Binc i -> sprintf "binc %d" i
      | Movestogo n -> sprintf "movestogo %d" n
      | Depth n -> sprintf "depth %d" n
      | Nodes n -> sprintf "nodes %d" n
      | Mate n -> sprintf "mate %d" n
      | Movetime t -> sprintf "movetime %d" t
      | Infinite -> "infinite"
  end

  type t =
    | Uci
    | Debug of [`on | `off]
    | Isready
    | Setoption of Option.t
    | Register of [`later | `namecode of string * string]
    | Ucinewgame
    | Position of [`fen of string | `startpos] * Move.t list
    | Go of Go.t
    | Stop
    | Ponderhit
    | Quit

  let to_string = function
    | Uci -> "uci"
    | Debug `on -> "debug on"
    | Debug `off -> "debug off"
    | Isready -> "isready"
    | Setoption opt -> "setoption " ^ Option.to_string opt
    | Register `later -> "register later"
    | Register (`namecode (name, code)) ->
      sprintf "register name %s code %s" name code
    | Ucinewgame -> "ucinewgame"
    | Position (`fen fen, moves) ->
      let moves = string_of_moves moves in
      let moves = if String.is_empty moves then moves else " " ^ moves in
      sprintf "position fen %s moves%s" fen moves
    | Position (`startpos, moves) ->
      let moves = string_of_moves moves in
      let moves = if String.is_empty moves then moves else " " ^ moves in
      sprintf "position startpos moves%s" moves
    | Go go -> "go " ^ Go.to_string go
    | Stop -> "stop"
    | Ponderhit -> "ponderhit"
    | Quit -> "quit"
end

module Send = struct
  module Option = struct
    module Spin = struct
      type t = {
        default : int;
        min : int;
        max : int;
      }

      let to_string {default; min; max} =
        sprintf "type spin default %d min %d max %d" default min max
    end

    module Check = struct
      type t = {
        default : bool;
      }

      let to_string {default} =
        sprintf "type check default %s" (Bool.to_string default)
    end

    module Combo = struct
      type t = {
        default : string;
        var : string list;
      }

      let to_string {default; var} =
        let default = sprintf "default %s" @@ string_of_string default in
        let var = List.map var ~f:(fun s -> "var " ^ string_of_string s) in
        sprintf "type combo %s" @@ String.concat (default :: var) ~sep:" "
    end

    module String = struct
      type t = {
        default : string
      }

      let to_string {default} =
        "type string default " ^ string_of_string default
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

    let to_string = function
      | Hash spin -> "name Hash " ^ Spin.to_string spin
      | NalimovPath string -> "name NalimovPath " ^ String.to_string string
      | NalimovCache spin -> "name NalimovCache " ^ Spin.to_string spin
      | Ponder check -> "name Ponder " ^ Check.to_string check
      | OwnBook check -> "name OwnBook " ^ Check.to_string check
      | MultiPV spin -> "name MultiPV " ^ Spin.to_string spin
      | UCI_ShowCurrLine check ->
        "name UCI_ShowCurrLine " ^ Check.to_string check
      | UCI_ShowRefutations check ->
        "name UCI_ShowRefutations " ^ Check.to_string check
      | UCI_LimitStrength check ->
        "name UCI_LimitStrength " ^ Check.to_string check
      | UCI_Elo spin -> "name UCI_Elo " ^ Spin.to_string spin
      | UCI_AnalyseMode check ->
        "name UCI_AnalyseMode " ^ Check.to_string check
      | UCI_Opponent string -> "name UCI_Opponent " ^ String.to_string string
      | UCI_EngineAbout string ->
        "name UCI_EngineAbout " ^ String.to_string string
      | UCI_ShredderbasesPath string ->
        "name UCI_ShredderbasesPath " ^ String.to_string string
      | UCI_SetPositionValue string ->
        "name UCI_SetPositionValue " ^ String.to_string string
  end

  module Bestmove = struct
    type t = {
      move : Move.t;
      ponder : Move.t option;
    }

    let to_string = function
      | {move; ponder = None} -> Format.asprintf "bestmove %a" Move.pp move
      | {move; ponder = Some ponder} ->
        Format.asprintf "bestmove %a ponder %a" Move.pp move Move.pp ponder
  end

  module Info = struct
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

    let string_of_bound = function
      | `lower -> "lowerbound"
      | `upper -> "upperbound"

    let to_string = function
      | Depth n -> sprintf "depth %d" n
      | Seldepth n -> sprintf "seldepth %d" n
      | Time t -> sprintf "time %d" t
      | Nodes n -> sprintf "nodes %d" n
      | Pv moves -> "pv " ^ string_of_moves moves
      | Multipv n -> sprintf "multipv %d" n
      | Score {cp; mate; bound} ->
        sprintf "score cp %f mate %d %s" cp mate (string_of_bound bound)
      | Currmove move -> "currmove " ^ Move.to_string move
      | Currmovenumber n -> sprintf "currmovenumber %d" n
      | Hashfull n -> sprintf "hashfull %d" n
      | Nps n -> sprintf "nps %d" n
      | Tbhits n -> sprintf "tbhits %d" n
      | Sbhits n -> sprintf "sbhits %d" n
      | Cpuload n -> sprintf "cpuload %d" n
      | String s -> "string " ^ s
      | Refutation moves -> "refutation " ^ string_of_moves moves
      | Currline {cpunr; moves} ->
        sprintf "currline %d %s" cpunr (string_of_moves moves)
  end

  type t =
    | Id of [`name of string | `author of string]
    | Uciok
    | Readyok
    | Bestmove of Bestmove.t
    | Copyprotection of [`checking | `ok | `error]
    | Registration of [`checking | `ok | `error]
    | Info of Info.t list
    | Option of Option.t

  let to_string = function
    | Id (`name name) -> "id name " ^ name
    | Id (`author author) -> "id author " ^ author
    | Uciok -> "uciok"
    | Readyok -> "readyok"
    | Bestmove bestmove -> Bestmove.to_string bestmove
    | Copyprotection `checking -> "copyprotection checking"
    | Copyprotection `ok -> "copyprotection ok"
    | Copyprotection `error -> "copyprotection error"
    | Registration `checking -> "registration checking"
    | Registration `ok -> "registration ok"
    | Registration `error -> "registration error"
    | Info info ->
      sprintf "info %s" @@
      String.concat ~sep:" " @@
      List.map info ~f:Info.to_string
    | Option opt -> "option " ^ Option.to_string opt
end

type t = Recv of Recv.t | Send of Send.t

let to_string = function
  | Recv recv -> Recv.to_string recv
  | Send send -> Send.to_string send
