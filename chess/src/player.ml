open Core_kernel

include Player_intf

let players = Hashtbl.create (module String)

let register player =
  let key = player#name in
  match Hashtbl.add players ~key ~data:player with
  | `Duplicate -> invalid_argf "Player %s is already registered" key ()
  | `Ok -> ()

let lookup = Hashtbl.find players
let enumerate () = Hashtbl.data players

let () =
  (* These players are meant to be extremely simple. As such, they
     shouldn't require any parameters (such as search limits). *)
  register @@ Player_random.create ();
  register @@ Player_same_color.create ();
  register @@ Player_opposite_color.create ();
  register @@ Player_cccp.create ();
  register @@ Player_huddle.create ();
  register @@ Player_swarm.create ();
  register @@ Player_minimize_opponent_moves.create ();
  register @@ Player_maximize_opponent_moves.create ();
  register @@ Player_suicide_king.create ();
  register @@ Player_pacifist.create ()
