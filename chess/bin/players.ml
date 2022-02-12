open Core_kernel
open Chess

(* We're using a map because we want the order to be consistent when
   displaying the available players. *)
let players = ref @@ Map.empty (module String)

let register player =
  let key = Player.name player in
  match Map.add !players ~key ~data:Player.(T player) with
  | `Duplicate -> invalid_argf "Player %s is already registered" key ()
  | `Ok m -> players := m

let lookup name = Map.find !players name
let enumerate () = Map.data !players
