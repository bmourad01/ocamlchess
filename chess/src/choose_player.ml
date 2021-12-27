open Core_kernel

let players = Hashtbl.of_alist_exn (module String) [
    "random", Player_random.create;
    "same-color", Player_same_color.create;
    "opposite-color", Player_opposite_color.create;
    "cccp", Player_cccp.create;
    "huddle", Player_huddle.create;
    "swarm", Player_swarm.create;
    "min-oppt-moves", Player_minimize_opponent_moves.create;
    "max-oppt-moves", Player_maximize_opponent_moves.create;
    "suicide-king", Player_suicide_king.create;
    "pacifist", Player_pacifist.create;
  ]

let choose ?(limits = None) name =
  match Hashtbl.find players name with
  | None -> invalid_arg @@ sprintf "Invalid player '%s'" name
  | Some (create : Player.create) -> create ~limits ()
