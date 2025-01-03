open Core_kernel [@@warning "-D"]

let concat = String.concat ~sep:" "
let concat_rev tok = concat @@ List.rev tok

let pp_moves ppf moves =
  let pp_sep ppf () = Format.fprintf ppf " " in
  Format.pp_print_list ~pp_sep Move.pp ppf moves

let tokens s =
  String.split s ~on:' ' |>
  List.filter ~f:(Fn.non String.is_empty)
