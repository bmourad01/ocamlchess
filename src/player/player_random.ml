open Core_kernel

let choose pos = match List.random_element @@ Position.legal_moves pos with
  | None -> raise Player.No_moves
  | Some m -> m

let create ?(limits = None) () = Player.create ~choose ~limits ()
