open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal_move
module Legals = Position.Legal_moves

let chebyshev sq sq' =
  let rank, file = Square.decomp sq in
  let rank', file' = Square.decomp sq' in
  max (abs (rank - rank')) (abs (file - file'))

let choose lms = match Legals.decomp lms with
  | [], _ -> raise Player.No_moves
  | moves, pos ->
    let active = Position.active pos in
    let enemy = Position.enemy pos in
    let enemy_board = Position.board_of_color pos enemy in
    let king = Position.king pos in
    let king_sq = Bb.(first_set_exn (king & enemy_board)) in
    Player.best_moves moves ~eval:(fun mv ->
        let pos = Legal.position mv in
        Position.find_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, _) -> acc - chebyshev king_sq sq) |>
        Option.return) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "swarm"
end
