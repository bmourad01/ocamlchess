open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let choose pos = function
  | [] -> raise Player.No_moves
  | moves ->
    let active = Position.active pos in
    Player.best_moves moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        let king = Position.king pos in
        let active_board = Position.board_of_color pos active in
        let enemy_board = Position.active_board pos in
        let k1 = Bb.(first_set_exn (king & active_board)) in
        let k2 = Bb.(first_set_exn (king & enemy_board)) in
        Some (-(Square.chebyshev k1 k2))) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "suicide-king"
end
