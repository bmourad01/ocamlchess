open Core_kernel

module Legal = Position.Legal

let choose _ moves = match List.random_element moves with
  | None -> raise Player.No_moves
  | Some m -> m

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "random"
end
