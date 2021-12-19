open Core_kernel

let choose ?(limits = None) s =
  let player = match s with
    | "random" -> new Player_random.cls ~limits ()
    | "same-color" -> new Player_same_color.cls ~limits ()
    | "opposite-color" -> new Player_opposite_color.cls ~limits ()
    | _ -> invalid_arg @@ sprintf "Invalid player %s" s in
  (player :> Player.t)
