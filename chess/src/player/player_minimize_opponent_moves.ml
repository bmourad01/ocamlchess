open Core_kernel

module Legal = Position.Legal_move
module Legals = Position.Legal_moves

let choose legals = match Legals.moves legals with
  | [] -> raise Player.No_moves
  | moves ->
    Player.best_moves moves ~eval:(fun mv ->
        let pos = Legal.position mv in
        Some (-(List.length @@ Legals.moves @@ Position.legal_moves pos))) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "min-oppt-moves"
end
