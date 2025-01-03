open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

module Type = Uci_send_option_type

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
