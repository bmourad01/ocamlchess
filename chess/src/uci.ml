open Core_kernel
open Monads.Std

let concat = String.concat ~sep:" "
let concat_rev tok = concat @@ List.rev tok
let string_of_moves moves = concat @@ List.map moves ~f:Move.to_string

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
      | {name; value = None} -> sprintf "name %s" name
      | {name; value = Some value} -> sprintf "name %s value %s" name value

    let of_tokens tok = match tok with
      | "name" :: tok ->
        let rec aux acc = function
          | [] -> Some {name = concat_rev acc; value = None}
          | "value" :: [] -> None
          | "value" :: rest -> begin
              match acc with
              | [] -> None
              | acc -> Some {
                  name = concat_rev acc;
                  value = Some (concat rest);
                }
            end
          | s :: rest -> aux (s :: acc) rest in
        aux [] tok
      | _ -> None

    let of_string s = of_tokens @@ tokens s
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
      | Searchmoves moves -> sprintf "searchmoves %s" @@ string_of_moves moves
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

    let of_tokens tok =
      let open Monad.Option.Syntax in
      match tok with
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

    let of_string s = of_tokens @@ tokens s 
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
    | Setoption opt -> sprintf "setoption %s" @@ Setoption.to_string opt
    | Register `later -> "register later"
    | Register (`namecode (name, code)) ->
      sprintf "register name %s code %s" name code
    | Ucinewgame -> "ucinewgame"
    | Position (`fen pos, moves) ->
      let moves =
        let s = string_of_moves moves in
        if String.is_empty s then s else sprintf " moves %s" s in
      sprintf "position fen %s%s" (Position.Fen.to_string pos) moves
    | Position (`startpos, moves) ->
      let moves =
        let s = string_of_moves moves in
        if String.is_empty s then s else sprintf " moves %s" s in
      sprintf "position startpos%s" moves
    | Go go -> sprintf "go %s" @@ Go.to_string go
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
    | "setoption" :: rest -> Setoption.of_tokens rest >>| fun o -> Setoption o
    | ["register"; "later"] -> Some (Register `later)
    | "register" :: "name" :: rest ->
      let rec aux acc = function
        | [] -> None
        | "code" :: [] -> None
        | "code" :: rest ->
          Some (Register (`namecode (concat_rev acc, concat rest)))
        | s :: rest -> aux (s :: acc) rest in
      aux [] rest
    | ["ucinewgame"] -> Some Ucinewgame
    | "position" :: "fen" :: p :: c :: cr :: ep :: h :: f :: rest ->
      let fen = concat [p; c; cr; ep; h; f] in
      Position.Fen.of_string fen ~validate:false |> Result.ok >>= fun pos ->
      begin match rest with
        | [] -> Some []
        | "moves" :: moves -> Monad.Option.List.map moves ~f:Move.of_string
        | _ -> None
      end >>| fun moves -> Position (`fen pos, moves)
    | ["position"; "startpos"] -> Some (Position (`startpos, []))
    | "position" :: "startpos" :: "moves" :: moves ->
      Monad.Option.List.map moves ~f:Move.of_string >>| fun moves ->
      Position (`startpos, moves)
    | "go" :: rest -> Go.of_tokens rest >>| fun go -> Go go
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
          sprintf "type combo %s" @@ concat (default :: var)
        | String default -> sprintf "type string default %s" default
        | Button -> "type button"

      let of_tokens tok =
        let open Monad.Option.Syntax in
        match tok with
        | ["type"; "spin"; "default"; default; "min"; min; "max"; max] ->
          int_of_string_opt default >>= fun default ->
          int_of_string_opt min >>= fun min ->
          int_of_string_opt max >>| fun max ->
          Spin {default; min; max}
        | ["type"; "check"; "default"; default] ->
          bool_of_string_opt default >>| fun default -> Check default
        | "type" :: "combo" :: "default" :: rest ->
          let rec aux_default acc = function
            | [] -> concat_rev acc, []
            | ("var" :: _) as var -> concat_rev acc, var
            | s :: rest -> aux_default (s :: acc) rest in
          let default, var = aux_default [] rest in
          let vars = ref [] in
          let rec aux_var acc = function
            | [] -> Some (vars := concat_rev acc :: !vars)
            | "var" :: [] -> None
            | "var" :: "var" :: _ -> None
            | "var" :: v :: rest ->
              begin match acc with
                | [] -> ()
                | acc -> vars := concat_rev acc :: !vars
              end;
              aux_var [v] rest
            | s :: rest -> aux_var (s :: acc) rest in
          aux_var [] var >>| fun () ->
          Combo {default; var = List.rev !vars}
        | "type" :: "string" :: "default" :: default ->
          (* Note: this value could be the empty string. *)
          Some (String (concat default))
        | ["type"; "button"] -> Some Button
        | _ -> None

      let of_string s = of_tokens @@ tokens s
    end

    type t = {
      name : string;
      typ : Type.t;
    } [@@deriving equal, compare, sexp]

    let to_string {name; typ} =
      sprintf "name %s %s" name @@ Type.to_string typ

    let of_tokens tok =
      let open Monad.Option.Syntax in
      match tok with
      | "name" :: rest ->
        let rec aux acc = function
          | [] -> None
          | ("type" :: rest) as tok ->
            Type.of_tokens tok >>| fun typ -> {name = concat_rev acc; typ}
          | s :: rest -> aux (s :: acc) rest in
        aux [] rest
      | _ -> None

    let of_string s = of_tokens @@ tokens s
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

    let of_tokens tok =
      let open Monad.Option.Syntax in
      match tok with
      | [move; "ponder"; ponder] ->
        Move.of_string move >>= fun move ->
        Move.of_string ponder >>| fun ponder ->
        {move; ponder = Some ponder}
      | [move] -> Move.of_string move >>| fun move -> {move; ponder = None}
      | _ -> None

    let of_string s = of_tokens @@ tokens s
  end

  module Info = struct
    type score = {
      cp : int;
      mate : int option;
      bound : [`lower | `upper] option;
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
        let mate = match mate with
          | Some mate -> sprintf " mate %d" mate
          | None -> "" in
        let bound = match bound with
          | Some bound -> sprintf " bound %s" @@ string_of_bound bound
          | None -> "" in
        sprintf "score cp %d%s%s" cp mate bound
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
      | "score" :: "cp" :: cp :: rest ->
        int_of_string_opt cp >>= fun cp ->
        begin match rest with
          | [] -> Some (None, None)
          | ["mate"; mate; "bound"; bound] ->
            int_of_string_opt mate >>= fun mate ->
            bound_of_string bound >>| fun bound ->
            Some mate, Some bound
          | ["bound"; bound; "mate"; mate] ->
            int_of_string_opt mate >>= fun mate ->
            bound_of_string bound >>| fun bound ->
            Some mate, Some bound
          | ["mate"; mate] ->
            int_of_string_opt mate >>| fun mate ->
            Some mate, None
          | ["bound"; bound] ->
            bound_of_string bound >>| fun bound ->
            None, Some bound
          | _ -> None
        end >>| fun (mate, bound) -> Score {cp; mate; bound}
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
    | Id (`name name) -> sprintf "id name %s" name
    | Id (`author author) -> sprintf "id author %s" author
    | Uciok -> "uciok"
    | Readyok -> "readyok"
    | Bestmove bestmove ->
      sprintf "bestmove %s" @@ Bestmove.to_string bestmove
    | Copyprotection `checking -> "copyprotection checking"
    | Copyprotection `ok -> "copyprotection ok"
    | Copyprotection `error -> "copyprotection error"
    | Registration `checking -> "registration checking"
    | Registration `ok -> "registration ok"
    | Registration `error -> "registration error"
    | Info info ->
      sprintf "info %s" @@ concat @@ List.map info ~f:Info.to_string
    | Option opt -> sprintf "option %s" @@ Option.to_string opt

  (* We have to write a special version of the `Info` parser that will
     keep looking ahead for more tokens. *)
  let parse_infos infos =
    let open Info in
    let open Monad.Option.Syntax in
    let moves_aux moves =
      let rec aux acc = function
        | [] -> Some (List.rev acc, [])
        | move :: rest -> match Move.of_string move with
          | None -> Some (List.rev acc, move :: rest)
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
      | "score" :: "cp" :: cp :: rest ->
        int_of_string_opt cp >>= fun cp ->
        begin match rest with
          | [] -> Some (None, None, [])
          | "mate" :: mate :: "bound" :: bound :: rest ->
            int_of_string_opt mate >>= fun mate ->
            bound_of_string bound >>| fun bound ->
            Some mate, Some bound, rest
          | "bound" :: bound :: "mate" :: mate :: rest ->
            int_of_string_opt mate >>= fun mate ->
            bound_of_string bound >>| fun bound ->
            Some mate, Some bound, rest
          | "mate" :: mate :: rest ->
            int_of_string_opt mate >>| fun mate ->
            Some mate, None, rest
          | "bound" :: bound :: rest ->
            bound_of_string bound >>| fun bound ->
            None, Some bound, rest
          | rest -> Some (None, None, rest)
        end >>= fun (mate, bound, rest) ->
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
    | "id" :: "name" :: name ->
      Some (Id (`name (concat name)))
    | "id" :: "author" :: author ->
      Some (Id (`author (concat author)))
    | ["uciok"] -> Some Uciok
    | ["readyok"] -> Some Readyok
    | "bestmove" :: rest ->
      Bestmove.of_tokens rest >>| fun bestmove -> Bestmove bestmove
    | ["copyprotection"; "checking"] -> Some (Copyprotection `checking)
    | ["copyprotection"; "ok"] -> Some (Copyprotection `ok)
    | ["copyprotection"; "error"] -> Some (Copyprotection `error)
    | ["registration"; "checking"] -> Some (Registration `checking)
    | ["registration"; "ok"] -> Some (Registration `ok)
    | ["registration"; "error"] -> Some (Registration `error)
    | "info" :: rest -> parse_infos rest >>| fun info -> Info info
    | "option" :: rest -> Option.of_tokens rest >>| fun opt -> Option opt
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
