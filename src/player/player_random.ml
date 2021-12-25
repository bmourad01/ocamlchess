open Core_kernel

module Lms = Position.Legal_moves

let choose lms = match List.random_element @@ Lms.moves lms with
  | None -> raise Player.No_moves
  | Some m -> m

let create ?(limits = None) () = Player.create ~choose ~limits ()
