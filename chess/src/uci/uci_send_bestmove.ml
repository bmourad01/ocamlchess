open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

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
