open Core_kernel [@@warning "-D"]

let concat = String.concat ~sep:" "
let concat_rev tok = concat @@ List.rev tok

let pp_moves ppf moves =
  let rec aux = function
    | [] -> ()
    | [m] -> Format.fprintf ppf "%a%!" Move.pp m
    | m :: ms ->
      Format.fprintf ppf "%a %!" Move.pp m;
      aux ms in
  aux moves

let tokens s =
  String.split s ~on:' ' |>
  List.filter ~f:(Fn.non String.is_empty)
