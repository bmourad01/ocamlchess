open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

module Setoption = Uci_recv_setoption
module Go = Uci_recv_go

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
