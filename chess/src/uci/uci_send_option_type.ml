open Core_kernel [@@warning "-D"]
open Monads.Std
open Uci_common

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
