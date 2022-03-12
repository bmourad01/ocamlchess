open Core_kernel
open Chess

(* We're using a map because we want the order to be consistent when
   displaying the available players. *)
let players = ref @@ Map.empty (module String)

let register name player =
  match Map.add !players ~key:name ~data:player with
  | `Duplicate -> invalid_argf "Player %s is already registered" name ()
  | `Ok m -> players := m

let lookup name =
  Map.find !players name |>
  Option.map ~f:(fun create -> create ())

let enumerate () =
  Map.to_sequence !players |>
  Sequence.map ~f:(fun (_, create) -> create ())
