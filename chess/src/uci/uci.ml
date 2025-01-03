open Core_kernel [@@warning "-D"]
open Monads.Std

module Recv = Uci_recv
module Send = Uci_send

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
