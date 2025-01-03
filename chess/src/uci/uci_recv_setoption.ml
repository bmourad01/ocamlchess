open Core_kernel [@@warning "-D"]
open Uci_common

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
