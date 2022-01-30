open Core_kernel

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves

type t = <
  choose : Position.legal list -> Position.legal;
  limits : limits option;
  name : string;
  desc : string;
>

(* We're using `Map` because we want the order to be consistent when displaying
   the available players. *)
let players = ref @@ Map.empty (module String)

let register : t -> unit = fun player ->
  let key = player#name in
  match Map.add !players ~key ~data:player with
  | `Duplicate -> invalid_argf "Player %s is already registered" key ()
  | `Ok m -> players := m

let lookup name = Map.find !players name
let enumerate () = Map.data !players
