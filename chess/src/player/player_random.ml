open Core_kernel

module Legals = Position.Legal_moves

let choose legals = match List.random_element @@ Legals.moves legals with
  | None -> raise Player.No_moves
  | Some m -> m

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "random"
end
