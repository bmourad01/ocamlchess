open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

type bound = Exact | Lower | Upper
[@@deriving equal, compare, sexp]

type score =
  | Mate of int
  | Cp of int * bound
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

let bound_of_string = function
  | "lowerbound" -> Some Lower
  | "upperbound" -> Some Upper
  | _ -> None

let pp ppf = function
  | Depth n -> Format.fprintf ppf "depth %d%!" n
  | Seldepth n -> Format.fprintf ppf "seldepth %d%!" n
  | Time t -> Format.fprintf ppf "time %d%!" t
  | Nodes n -> Format.fprintf ppf "nodes %d%!" n
  | Pv moves -> Format.fprintf ppf "pv %a%!" pp_moves moves
  | Multipv n -> Format.fprintf ppf "multipv %d%!" n
  | Score (Mate n) -> Format.fprintf ppf "score mate %d%!" n
  | Score (Cp (cp, Exact)) ->
    Format.fprintf ppf "score cp %d%!" cp
  | Score (Cp (cp, Lower)) ->
    Format.fprintf ppf "score cp %d bound lowerbound%!" cp
  | Score (Cp (cp, Upper)) ->
    Format.fprintf ppf "score cp %d bound upperbound%!" cp
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
    Score (Cp (cp, Exact))
  | ["score"; "cp"; cp; "bound"; bound] ->
    int_of_string_opt cp >>= fun cp ->
    bound_of_string bound >>| fun bound ->
    Score (Cp (cp, bound))
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
