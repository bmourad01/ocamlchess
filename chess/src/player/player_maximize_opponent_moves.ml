open Core_kernel

module Legal = Position.Legal

let choose _ = function
  | [] -> raise Player.No_moves
  | moves ->
    Player.best_moves moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Some (List.length @@ Position.legal_moves pos)) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "max-oppt-moves"
end
