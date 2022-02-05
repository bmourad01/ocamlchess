open Core_kernel

type limits = {
  depth : int;
  nodes : int;
}

exception No_moves

type 'a choice =
  limits option -> 'a -> Position.legal list -> Position.legal * 'a

type 'a t = {
  choice : 'a choice;
  limits : limits option;
  name : string;
  desc : string;
  state : 'a;
} [@@deriving fields]

type e = T : 'a t -> e

let create ?(limits = None) ~choice ~name ~desc ~state () =
  Fields.create ~choice ~limits ~name ~desc ~state

let choose {choice; limits; state; _} pos =
  match Position.legal_moves pos with
  | [] -> raise No_moves
  | moves -> choice limits state moves

let set_state player state = {player with state}

(* We're using a map because we want the order to be consistent when
   displaying the available players. *)
let players = ref @@ Map.empty (module String)

let register : type a. a t -> unit = fun player ->
  let key = player.name in
  match Map.add !players ~key ~data:(T player) with
  | `Duplicate -> invalid_argf "Player %s is already registered" key ()
  | `Ok m -> players := m

let lookup name = Map.find !players name
let enumerate () = Map.data !players
