open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let choose pos = function
  | [] -> raise Player.No_moves
  | moves ->
    let active = Position.active pos in
    let enemy = Position.enemy pos in
    let enemy_board = Position.board_of_color pos enemy in
    let king = Position.king pos in
    let king_sq = Bb.(first_set_exn (king & enemy_board)) in
    Player.best_moves moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        Position.find_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, _) ->
            acc - Square.chebyshev king_sq sq) |>
        Option.return) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "swarm"
end
