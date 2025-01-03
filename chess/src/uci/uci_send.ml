open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

module Option = Uci_send_option
module Bestmove = Uci_send_bestmove
module Info = Uci_send_info

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
        | [] -> Some (Exact, [])
        | "bound" :: bound :: rest ->
          bound_of_string bound >>| fun bound ->
          bound, rest
        | rest -> Some (Exact, rest)
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
