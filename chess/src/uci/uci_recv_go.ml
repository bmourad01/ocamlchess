open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

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
