open Core_kernel [@@warning "-D"]
open Position_common

type t = histogram [@@deriving compare, equal, sexp]

let empty = Int64.Map.empty
let singleton pos = Int64.Map.singleton pos.hash 1

let incr h pos = Map.update h pos.hash ~f:(function
    | Some n -> n + 1
    | None -> 1)

let decr h pos = Map.change h pos.hash ~f:(function
    | None | Some 1 -> None
    | Some n -> Some (n - 1))

let frequency h pos =
  Map.find h pos.hash |> Option.value ~default:0

let to_sequence h = Map.to_sequence h
