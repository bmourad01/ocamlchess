open Core_kernel
open Monads.Std

let string_of_moves moves =
  List.map moves ~f:Move.to_string |> String.concat ~sep:" "

let tokens s =
  String.split s ~on:' ' |>
  List.filter ~f:(Fn.non String.is_empty)

module Recv = struct
  module Setoption = struct
    type t = {
      name : string;
      value : string option;
    } [@@deriving equal, compare, sexp]

    let to_string = function
      | {name; value = None} -> name
      | {name; value = Some value} -> sprintf "%s value %s" name value

    let of_string s = match tokens s with
      | [name; "value"; value] -> Some {name; value = Some value}
      | [name] -> Some {name; value = None}
      | _ -> None
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
    [@@deriving equal, compare, sexp]

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

    let of_string s =
      let open Monad.Option.Syntax in
      match tokens s with
      | "searchmoves" :: moves ->
        Monad.Option.List.map moves ~f:Move.of_string >>= begin function
          | [] -> None
          | moves -> Some (Searchmoves moves)
        end
      | ["ponder"] -> Some Ponder
      | ["wtime"; t] -> int_of_string_opt t >>| fun t -> Wtime t
      | ["btime"; t] -> int_of_string_opt t >>| fun t -> Btime t
      | ["movestogo"; n] -> int_of_string_opt n >>| fun n -> Movestogo n
      | ["depth"; n] -> int_of_string_opt n >>| fun n -> Depth n
      | ["nodes"; n] -> int_of_string_opt n >>| fun n -> Nodes n
      | ["mate"; n] -> int_of_string_opt n >>| fun n -> Mate n
      | ["movetime"; t] -> int_of_string_opt t >>| fun t -> Movetime t
      | ["infinite"] -> Some Infinite
      | _ -> None
  end

  type t =
    | Uci
    | Debug of [`on | `off]
    | Isready
    | Setoption of Setoption.t
    | Register of [`later | `namecode of string * string]
    | Ucinewgame
    | Position of [`fen of Position.t | `startpos] * Move.t list
    | Go of Go.t
    | Stop
    | Ponderhit
    | Quit
  [@@deriving equal, compare, sexp]

  let to_string = function
    | Uci -> "uci"
    | Debug `on -> "debug on"
    | Debug `off -> "debug off"
    | Isready -> "isready"
    | Setoption opt -> "setoption " ^ Setoption.to_string opt
    | Register `later -> "register later"
    | Register (`namecode (name, code)) ->
      sprintf "register name %s code %s" name code
    | Ucinewgame -> "ucinewgame"
    | Position (`fen pos, moves) ->
      let moves =
        let s = string_of_moves moves in
        if String.is_empty s then s else " moves " ^ s in
      sprintf "position fen %s%s" (Position.Fen.to_string pos) moves
    | Position (`startpos, moves) ->
      let moves =
        let s = string_of_moves moves in
        if String.is_empty s then s else " moves " ^ s in
      sprintf "position startpos%s" moves
    | Go go -> "go " ^ Go.to_string go
    | Stop -> "stop"
    | Ponderhit -> "ponderhit"
    | Quit -> "quit"

  let of_string s =
    let open Monad.Option.Syntax in
    match tokens s with
    | ["uci"] -> Some Uci
    | ["debug"; "on"] -> Some (Debug `on)
    | ["debug"; "off"] -> Some (Debug `off)
    | ["isready"] -> Some Isready
    | ("setoption" as c) :: _ ->
      let n = String.length c + 1 in
      let s = String.drop_prefix s n in
      Setoption.of_string s >>| fun o -> Setoption o
    | ["register"; "later"] -> Some (Register `later)
    | ["register"; "name"; name; "code"; code] ->
      Some (Register (`namecode (name, code)))
    | ["ucinewgame"] -> Some Ucinewgame
    | "position" :: "fen" :: p :: c :: cr :: ep :: h :: f :: rest ->
      let fen = String.concat ~sep:" " [p; c; cr; ep; h; f] in
      Position.Fen.of_string fen |> Result.ok >>= fun pos ->
      begin match rest with
        | [] -> Some []
        | "moves" :: moves -> Monad.Option.List.map moves ~f:Move.of_string
        | _ -> None
      end >>| fun moves -> Position (`fen pos, moves)
    | ["position"; "startpos"] -> Some (Position (`startpos, []))
    | "position" :: "startpos" :: "moves" :: moves ->
      Monad.Option.List.map moves ~f:Move.of_string >>| fun moves ->
      Position (`startpos, moves)
    | ("go" as c) :: _ ->
      let n = String.length c + 1 in
      let s = String.drop_prefix s n in
      Go.of_string s >>| fun go -> Go go
    | ["stop"] -> Some Stop
    | ["ponderhit"] -> Some Ponderhit
    | ["quit"] -> Some Quit
    | _ -> None
end

module Send = struct
  module Option = struct
    module Type = struct
      type spin = {
        default : int;
        min : int;
        max : int;
      } [@@deriving equal, compare, sexp]

      type combo = {
        default : string;
        var : string list;
      } [@@deriving equal, compare, sexp]

      type t =
        | Spin of spin
        | Check of bool
        | Combo of combo
        | String of string
        | Button
      [@@deriving equal, compare, sexp]

      let to_string = function
        | Spin {default; min; max} ->
          sprintf "type spin default %d min %d max %d" default min max
        | Check default -> sprintf "type check default %b" default
        | Combo {default; var} ->
          let default = sprintf "default %s" default in
          let var = List.map var ~f:(sprintf "var %s") in
          sprintf "type combo %s" @@ String.concat (default :: var) ~sep:" "
        | String default -> sprintf "type string default %s" default
        | Button -> "type button"

      let of_string s =
        let open Monad.Option.Syntax in
        match tokens s with
        | ["type"; "spin"; "default"; default; "min"; min; "max"; max] ->
          int_of_string_opt default >>= fun default ->
          int_of_string_opt min >>= fun min ->
          int_of_string_opt max >>| fun max ->
          Spin {default; min; max}
        | ["type"; "check"; "default"; default] ->
          bool_of_string_opt default >>| fun default -> Check default
        | "type" :: "combo" :: "default" :: default :: var ->
          let rec aux acc = function
            | [] -> Some (List.rev acc)
            | "var" :: var :: rest -> aux (var :: acc) rest
            | _ -> None in
          aux [] var >>| fun var -> Combo {default; var}
        | ["type"; "string"; "default"; default] -> Some (String default)
        | ["type"; "button"] -> Some Button
        | _ -> None
    end

    type t = {
      name : string;
      typ : Type.t;
    } [@@deriving equal, compare, sexp]

    let to_string {name; typ} =
      sprintf "name %s %s" name @@ Type.to_string typ

    let of_string s =
      let open Monad.Option.Syntax in
      match tokens s with
      | ("name" as c) :: name :: _ ->
        let n = String.length c + String.length name + 2 in
        let s = String.drop_prefix s n in
        Type.of_string s >>| fun typ -> {name; typ}
      | _ -> None
  end

  module Bestmove = struct
    type t = {
      move : Move.t;
      ponder : Move.t option;
    } [@@deriving equal, compare, sexp]

    let to_string = function
      | {move; ponder = None} -> Format.asprintf "%a" Move.pp move
      | {move; ponder = Some ponder} ->
        Format.asprintf "%a ponder %a" Move.pp move Move.pp ponder

    let of_string s =
      let open Monad.Option.Syntax in
      match tokens s with
      | [move; "ponder"; ponder] ->
        Move.of_string move >>= fun move ->
        Move.of_string ponder >>| fun ponder ->
        {move; ponder = Some ponder}
      | [move] -> Move.of_string move >>| fun move -> {move; ponder = None}
      | _ -> None
  end

  module Info = struct
    type score = {
      cp : float;
      mate : int;
      bound : [`lower | `upper];
    } [@@deriving equal, compare, sexp]

    type currline = {
      cpunr : int;
      moves : Move.t list;
    } [@@deriving equal, compare, sexp]

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
    [@@deriving equal, compare, sexp]

    let string_of_bound = function
      | `lower -> "lowerbound"
      | `upper -> "upperbound"

    let bound_of_string = function
      | "lowerbound" -> Some `lower
      | "upperbound" -> Some `upper
      | _ -> None

    let to_string = function
      | Depth n -> sprintf "depth %d" n
      | Seldepth n -> sprintf "seldepth %d" n
      | Time t -> sprintf "time %d" t
      | Nodes n -> sprintf "nodes %d" n
      | Pv moves -> sprintf "pv %s" @@ string_of_moves moves
      | Multipv n -> sprintf "multipv %d" n
      | Score {cp; mate; bound} ->
        sprintf "score cp %f mate %d %s" cp mate @@ string_of_bound bound
      | Currmove move -> sprintf "currmove %s" @@ Move.to_string move
      | Currmovenumber n -> sprintf "currmovenumber %d" n
      | Hashfull n -> sprintf "hashfull %d" n
      | Nps n -> sprintf "nps %d" n
      | Tbhits n -> sprintf "tbhits %d" n
      | Sbhits n -> sprintf "sbhits %d" n
      | Cpuload n -> sprintf "cpuload %d" n
      | String s -> sprintf "string %s" s
      | Refutation moves -> sprintf "refutation %s" @@ string_of_moves moves
      | Currline {cpunr; moves} ->
        sprintf "currline %d %s" cpunr @@ string_of_moves moves

    let of_string s =
      let open Monad.Option.Syntax in
      let moves_aux moves =
        Monad.Option.List.map moves ~f:Move.of_string >>= function
        | [] -> None | moves -> Some moves in
      match tokens s with
      | ["depth"; n] -> int_of_string_opt n >>| fun n -> Depth n
      | ["seldepth"; n] -> int_of_string_opt n >>| fun n -> Seldepth n
      | ["time"; t] -> int_of_string_opt t >>| fun t -> Time t
      | ["nodes"; n] -> int_of_string_opt n >>| fun n -> Nodes n
      | "pv" :: moves -> moves_aux moves >>| fun moves -> Pv moves
      | ["multipv"; n] -> int_of_string_opt n >>| fun n -> Multipv n
      | ["score"; "cp"; cp; "mate"; mate; bound] ->
        float_of_string_opt cp >>= fun cp ->
        int_of_string_opt mate >>= fun mate ->
        bound_of_string bound >>| fun bound ->
        Score {cp; mate; bound}
      | ["currmove"; move] ->
        Move.of_string move >>| fun move -> Currmove move
      | ["currmovenumber"; n] ->
        int_of_string_opt n >>| fun n -> Currmovenumber n
      | ["hashfull"; n] -> int_of_string_opt n >>| fun n -> Hashfull n
      | ["nps"; n] -> int_of_string_opt n >>| fun n -> Nps n
      | ["tbhits"; n] -> int_of_string_opt n >>| fun n -> Tbhits n
      | ["sbhits"; n] -> int_of_string_opt n >>| fun n -> Sbhits n
      | ["cpuload"; n] -> int_of_string_opt n >>| fun n -> Cpuload n
      | ["string"; s] -> Some (String s)
      | "refutation" :: moves ->
        moves_aux moves >>| fun moves -> Refutation moves
      | "currline" :: cpunr :: moves ->
        int_of_string_opt cpunr >>= fun cpunr ->
        moves_aux moves >>| fun moves ->
        Currline {cpunr; moves}
      | _ -> None
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
  [@@deriving equal, compare, sexp]

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

  (* We have to write a special version of the `Info` parser that will
     keep looking ahead for more tokens. *)
  let parse_infos infos =
    let open Info in
    let open Monad.Option.Syntax in
    let moves_aux moves =
      let rec aux acc = function
        | [] -> Some (List.rev acc, [])
        | move :: rest -> match Move.of_string move with
          | None -> Some (List.rev acc, rest)
          | Some move -> aux (move :: acc) rest in
      aux [] moves >>= function
      | [], _ -> None | res -> Some res in
    let rec aux acc = function
      | [] -> Some (List.rev acc)
      | "depth" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Depth n :: acc) rest
      | "seldepth" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Seldepth n :: acc) rest
      | "time" :: t :: rest ->
        int_of_string_opt t >>= fun t ->
        aux (Time t :: acc) rest
      | "nodes" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Nodes n :: acc) rest
      | "pv" :: rest ->
        moves_aux rest >>= fun (moves, rest) ->
        aux (Pv moves :: acc) rest
      | "multipv" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Multipv n :: acc) rest
      | "score" :: "cp" :: cp :: "mate" :: mate :: bound :: rest ->
        float_of_string_opt cp >>= fun cp ->
        int_of_string_opt mate >>= fun mate ->
        bound_of_string bound >>= fun bound ->
        aux (Score {cp; mate; bound} :: acc) rest
      | "currmove" :: move :: rest ->
        Move.of_string move >>= fun move ->
        aux (Currmove move :: acc) rest
      | "currmovenumber" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Currmovenumber n :: acc) rest
      | "hashfull" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Hashfull n :: acc) rest
      | "nps" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Nps n :: acc) rest
      | "tbhits" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Tbhits n :: acc) rest
      | "sbhits" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Sbhits n :: acc) rest
      | "cpuload" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Cpuload n :: acc) rest
      | "string" :: s :: rest -> aux (String s :: acc) rest
      | "refutation" :: rest ->
        moves_aux rest >>= fun (moves, rest) ->
        aux (Refutation moves :: acc) rest
      | "currline" :: cpunr :: rest ->
        int_of_string_opt cpunr >>= fun cpunr ->
        moves_aux rest >>= fun (moves, rest) ->
        aux (Currline {cpunr; moves} :: acc) rest
      | _ -> None in
    aux [] infos >>= function
    | [] -> None | infos -> Some infos

  let of_string s =
    let open Monad.Option.Syntax in
    match tokens s with
    | ["id"; "name"; name] -> Some (Id (`name name))
    | ["id"; "author"; author] -> Some (Id (`author author))
    | ["uciok"] -> Some Uciok
    | ["readyok"] -> Some Readyok
    | ("bestmove" as c) :: _ ->
      let n = String.length c + 1 in
      let s = String.drop_prefix s n in
      Bestmove.of_string s >>| fun bestmove ->
      Bestmove bestmove
    | ["copyprotection"; "checking"] -> Some (Copyprotection `checking)
    | ["copyprotection"; "ok"] -> Some (Copyprotection `ok)
    | ["copyprotection"; "error"] -> Some (Copyprotection `error)
    | ["registration"; "checking"] -> Some (Registration `checking)
    | ["registration"; "ok"] -> Some (Registration `ok)
    | ["registration"; "error"] -> Some (Registration `error)
    | "info" :: rest -> parse_infos rest >>| fun info -> Info info
    | ("option" as c) :: _ ->
      let n = String.length c + 1 in
      let s = String.drop_prefix s n in
      Option.of_string s >>| fun opt -> Option opt
    | _ -> None
end

type t =
  | Recv of Recv.t
  | Send of Send.t
[@@deriving equal, compare, sexp]

let to_string = function
  | Recv recv -> Recv.to_string recv
  | Send send -> Send.to_string send

let of_string s = match Recv.of_string s with
  | None -> Send.of_string s |> Option.map ~f:(fun send -> Send send)
  | Some recv -> Some (Recv recv)
