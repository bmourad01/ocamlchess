open Core_kernel
open Monads.Std

let concat = String.concat ~sep:" "
let concat_rev tok = concat @@ List.rev tok

let pp_moves ppf moves =
  let rec aux = function
    | [] -> ()
    | [m] -> Format.fprintf ppf "%a%!" Move.pp m
    | m :: ms ->
      Format.fprintf ppf "%a %!" Move.pp m;
      aux ms in
  aux moves

let tokens s =
  String.split s ~on:' ' |>
  List.filter ~f:(Fn.non String.is_empty)

module Recv = struct
  module Setoption = struct
    type t = {
      name  : string;
      value : string option;
    } [@@deriving equal, compare, sexp]

    let pp ppf = function
      | {name; value = None} -> Format.fprintf ppf "name %s%!" name
      | {name; value = Some value} ->
        Format.fprintf ppf "name %s value %s%!" name value

    let to_string t = Format.asprintf "%a%!" pp t

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

    let pp ppf = function
      | Searchmoves moves ->
        Format.fprintf ppf "searchmoves %a%!" pp_moves moves
      | Ponder -> Format.fprintf ppf "ponder%!"
      | Wtime t -> Format.fprintf ppf "wtime %d%!" t
      | Btime t -> Format.fprintf ppf "btime %d%!" t
      | Winc i -> Format.fprintf ppf "winc %d%!" i
      | Binc i -> Format.fprintf ppf "binc %d%!" i
      | Movestogo n -> Format.fprintf ppf "movestogo %d%!" n
      | Depth n -> Format.fprintf ppf "depth %d%!" n
      | Nodes n -> Format.fprintf ppf "nodes %d%!" n
      | Mate n -> Format.fprintf ppf "mate %d%!" n
      | Movetime t -> Format.fprintf ppf "movetime %d%!" t
      | Infinite -> Format.fprintf ppf "infinite%!"

    let to_string t = Format.asprintf "%a%!" pp t

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
      | ["winc"; n] -> int_of_string_opt n >>| fun n -> Winc n
      | ["binc"; n] -> int_of_string_opt n >>| fun n -> Binc n
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
    | Go of Go.t list
    | Stop
    | Ponderhit
    | Quit
  [@@deriving equal, compare, sexp]

  let pp_go ppf go =
    let rec aux = function
      | [] -> ()
      | [g] -> Format.fprintf ppf "%a%!" Go.pp g
      | g :: gs ->
        Format.fprintf ppf "%a %!" Go.pp g;
        aux gs in
    aux go

  let pp ppf = function
    | Uci -> Format.fprintf ppf "uci%!"
    | Debug `on -> Format.fprintf ppf "debug on%!"
    | Debug `off -> Format.fprintf ppf "debug off%!"
    | Isready -> Format.fprintf ppf "isready%!"
    | Setoption opt -> Format.fprintf ppf "setoption %a%!" Setoption.pp opt
    | Register `later -> Format.fprintf ppf "register later%!"
    | Register (`namecode (name, code)) ->
      Format.fprintf ppf "register name %s code %s%!" name code
    | Ucinewgame -> Format.fprintf ppf "ucinewgame%!"
    | Position (`fen pos, []) ->
      Format.fprintf ppf "position fen %a%!" Position.pp pos
    | Position (`fen pos, moves) ->
      Format.fprintf ppf "position fen %a moves %a%!"
        Position.pp pos pp_moves moves
    | Position (`startpos, []) -> Format.fprintf ppf "position startpos%!"
    | Position (`startpos, moves) ->
      Format.fprintf ppf "position startpos moves %a%!" pp_moves moves
    | Go go -> Format.fprintf ppf "go %a%!" pp_go go
    | Stop -> Format.fprintf ppf "stop%!"
    | Ponderhit -> Format.fprintf ppf "ponderhit%!"
    | Quit -> Format.fprintf ppf "quit%!"

  let to_string t = Format.asprintf "%a%!" pp t

  let parse_gos gos =
    let open Go in
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
      | "searchmoves" :: rest ->
        moves_aux rest >>= fun (moves, rest) ->
        aux (Searchmoves moves :: acc) rest
      | "ponder" :: rest -> aux (Ponder :: acc) rest
      | "wtime" :: t :: rest ->
        int_of_string_opt t >>= fun t ->
        aux (Wtime t :: acc) rest
      | "btime" :: t :: rest ->
        int_of_string_opt t >>= fun t ->
        aux (Btime t :: acc) rest
      | "winc" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Winc n :: acc) rest
      | "binc" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Binc n :: acc) rest
      | "movestogo" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Movestogo n :: acc) rest
      | "depth" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Depth n :: acc) rest
      | "nodes" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Nodes n :: acc) rest
      | "mate" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Mate n :: acc) rest
      | "movetime" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Movetime n :: acc) rest
      | "infinite" :: rest -> aux (Infinite :: acc) rest
      | _ -> None in
    aux [] gos

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
      Position.Fen.of_string fen ~validate:false |>
      Result.ok >>= fun pos -> begin match rest with
        | [] -> Some []
        | "moves" :: moves -> Monad.Option.List.map moves ~f:Move.of_string
        | _ -> None
      end >>| fun moves -> Position (`fen pos, moves)
    | ["position"; "startpos"] -> Some (Position (`startpos, []))
    | "position" :: "startpos" :: "moves" :: moves ->
      Monad.Option.List.map moves ~f:Move.of_string >>| fun moves ->
      Position (`startpos, moves)
    | "go" :: gos -> parse_gos gos >>| fun go -> Go go
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
        min     : int;
        max     : int;
      } [@@deriving equal, compare, sexp]

      type combo = {
        default : string;
        var     : string list;
      } [@@deriving equal, compare, sexp]

      type t =
        | Spin of spin
        | Check of bool
        | Combo of combo
        | String of string
        | Button
      [@@deriving equal, compare, sexp]

      let pp_var ppf var =
        let rec aux = function
          | [] -> ()
          | [v] -> Format.fprintf ppf "var %s%!" v
          | v :: vs ->
            Format.fprintf ppf "var %s %!" v;
            aux vs in
        aux var

      let pp ppf = function
        | Spin {default; min; max} ->
          Format.fprintf ppf "type spin default %d min %d max %d%!"
            default min max
        | Check default ->
          Format.fprintf ppf "type check default %b%!" default
        | Combo {default; var = []} ->
          Format.fprintf ppf "type combo default %s%!" default
        | Combo {default; var} ->
          Format.fprintf ppf "type combo default %s %a%!" default pp_var var
        | String default ->
          Format.fprintf ppf "type string default %s%!" default
        | Button -> Format.fprintf ppf "type button%!"

      let to_string t = Format.asprintf "%a%!" pp t

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

      let clamp n {min; max; _} = Int.clamp_exn n ~min ~max
      let is_var v {var; _} = List.mem var v ~equal:String.equal
    end

    type t = {
      name : string;
      typ  : Type.t;
    } [@@deriving equal, compare, sexp]

    let pp ppf {name; typ} =
      Format.fprintf ppf "name %s %a%!" name Type.pp typ

    let to_string t = Format.asprintf "%a%!" pp t

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
      move   : Move.t;
      ponder : Move.t option;
    } [@@deriving equal, compare, sexp]

    let pp ppf = function
      | {move; ponder = None} -> Format.fprintf ppf "%a" Move.pp move
      | {move; ponder = Some ponder} ->
        Format.fprintf ppf "%a ponder %a" Move.pp move Move.pp ponder

    let to_string t = Format.asprintf "%a%!" pp t

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
    type score =
      | Mate of int
      | Cp of int * [`lower | `upper] option
    [@@deriving equal, compare, sexp]

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

    let pp_bound ppf = function
      | `lower -> Format.fprintf ppf "lowerbound%!"
      | `upper -> Format.fprintf ppf "upperbound%!"

    let bound_of_string = function
      | "lowerbound" -> Some `lower
      | "upperbound" -> Some `upper
      | _ -> None

    let pp ppf = function
      | Depth n -> Format.fprintf ppf "depth %d%!" n
      | Seldepth n -> Format.fprintf ppf "seldepth %d%!" n
      | Time t -> Format.fprintf ppf "time %d%!" t
      | Nodes n -> Format.fprintf ppf "nodes %d%!" n
      | Pv moves -> Format.fprintf ppf "pv %a%!" pp_moves moves
      | Multipv n -> Format.fprintf ppf "multipv %d%!" n
      | Score (Mate n) -> Format.fprintf ppf "score mate %d%!" n
      | Score (Cp (cp, None)) -> Format.fprintf ppf "score cp %d%!" cp
      | Score (Cp (cp, Some bound)) ->
        Format.fprintf ppf "score cp %d bound %a%!" cp pp_bound bound
      | Currmove move -> Format.fprintf ppf "currmove %a%!" Move.pp move
      | Currmovenumber n -> Format.fprintf ppf "currmovenumber %d%!" n
      | Hashfull n -> Format.fprintf ppf "hashfull %d%!" n
      | Nps n -> Format.fprintf ppf "nps %d%!" n
      | Tbhits n -> Format.fprintf ppf "tbhits %d%!" n
      | Sbhits n -> Format.fprintf ppf "sbhits %d%!" n
      | Cpuload n -> Format.fprintf ppf "cpuload %d%!" n
      | String s -> Format.fprintf ppf "string %s%!" s
      | Refutation moves ->
        Format.fprintf ppf "refutation %a%!" pp_moves moves
      | Currline {cpunr; moves} ->
        Format.fprintf ppf "currline %d %a%!" cpunr pp_moves moves

    let to_string t = Format.asprintf "%a%!" pp t

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
      | ["score"; "mate"; n] ->
        int_of_string_opt n >>| fun n ->
        Score (Mate n)
      | ["score"; "cp"; cp] ->
        int_of_string_opt cp >>| fun cp ->
        Score (Cp (cp, None))
      | ["score"; "cp"; cp; "bound"; bound] ->
        int_of_string_opt cp >>= fun cp ->
        bound_of_string bound >>| fun bound ->
        Score (Cp (cp, Some bound))
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
    | Bestmove of Bestmove.t option
    | Copyprotection of [`checking | `ok | `error]
    | Registration of [`checking | `ok | `error]
    | Info of Info.t list
    | Option of Option.t
  [@@deriving equal, compare, sexp]

  let pp_info ppf info =
    let rec aux = function
      | [] -> ()
      | [i] -> Format.fprintf ppf "%a%!" Info.pp i
      | i :: is ->
        Format.fprintf ppf "%a %!" Info.pp i;
        aux is in
    aux info

  let pp ppf = function
    | Id (`name name) -> Format.fprintf ppf "id name %s%!" name
    | Id (`author author) -> Format.fprintf ppf "id author %s%!" author
    | Uciok -> Format.fprintf ppf "uciok%!"
    | Readyok -> Format.fprintf ppf "readyok%!"
    | Bestmove None -> Format.fprintf ppf "bestmove (none)%!"
    | Bestmove (Some bestmove) ->
      Format.fprintf ppf "bestmove %a%!" Bestmove.pp bestmove
    | Copyprotection `checking ->
      Format.fprintf ppf "copyprotection checking%!"
    | Copyprotection `ok ->
      Format.fprintf ppf "copyprotection ok%!"
    | Copyprotection `error ->
      Format.fprintf ppf "copyprotection error%!"
    | Registration `checking ->
      Format.fprintf ppf "registration checking%!"
    | Registration `ok ->
      Format.fprintf ppf "registration ok%!"
    | Registration `error ->
      Format.fprintf ppf "registration error%!"
    | Info info -> Format.fprintf ppf "info %a%!" pp_info info
    | Option opt -> Format.fprintf ppf "option %a%!" Option.pp opt

  let to_string t = Format.asprintf "%a%!" pp t

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
      | "score" :: "mate" :: n :: rest ->
        int_of_string_opt n >>= fun n ->
        aux (Score (Mate n) :: acc) rest
      | "score" :: "cp" :: cp :: rest ->
        int_of_string_opt cp >>= fun cp ->
        begin match rest with
          | [] -> Some (None, [])
          | "bound" :: bound :: rest ->
            bound_of_string bound >>| fun bound ->
            Some bound, rest
          | rest -> Some (None, rest)
        end >>= fun (bound, rest) ->
        aux (Score (Cp (cp, bound)) :: acc) rest
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
    | "bestmove" :: "(none)" :: [] -> Some (Bestmove None)
    | "bestmove" :: rest ->
      Bestmove.of_tokens rest >>| fun bestmove ->
      Bestmove (Some bestmove)
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

let pp ppf = function
  | Recv recv -> Format.fprintf ppf "%a%!" Recv.pp recv
  | Send send -> Format.fprintf ppf "%a%!" Send.pp send

let to_string t = Format.asprintf "%a%!" pp t

let of_string s = match Recv.of_string s with
  | None -> Send.of_string s |> Option.map ~f:(fun send -> Send send)
  | Some recv -> Some (Recv recv)
