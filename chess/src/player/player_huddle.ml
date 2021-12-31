open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal

let choose pos = function
  | [] -> raise Player.No_moves
  | moves ->
    let active = Position.active pos in
    Player.best_moves moves ~eval:(fun m ->
        let pos = Legal.new_position m in
        let active_board = Position.board_of_color pos active in
        let king = Position.king pos in
        let king_sq = Bb.(first_set_exn (king & active_board)) in
        Position.collect_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, _) ->
            acc - Square.chebyshev king_sq sq) |>
        Option.return) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method limits = limits
  method choose = choose
  method name = "huddle"
end
