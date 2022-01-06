open Core_kernel

let choose _ moves = match List.random_element moves with
  | None -> raise Player_intf.No_moves
  | Some m -> m

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "random"
end
