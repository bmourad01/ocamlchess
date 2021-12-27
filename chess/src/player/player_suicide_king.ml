open Core_kernel

module Bb = Bitboard
module Legal = Position.Legal_move
module Legals = Position.Legal_moves

let chebyshev sq sq' =
  let rank, file = Square.decomp sq in
  let rank', file' = Square.decomp sq' in
  max (abs (rank - rank')) (abs (file - file'))

let choose legals = match Legals.decomp legals with
  | [], _ -> raise Player.No_moves
  | moves, pos ->
    let active = Position.active pos in
    Player.best_moves moves ~eval:(fun mv ->
        let pos = Legal.position mv in
        let king = Position.king pos in
        let active_board = Position.board_of_color pos active in
        let enemy_board = Position.active_board pos in
        let k1 = Bb.(first_set_exn (king & active_board)) in
        let k2 = Bb.(first_set_exn (king & enemy_board)) in
        Some (-(chebyshev k1 k2))) |>
    List.random_element_exn

let create ?(limits = None) () = object
  method choose = choose
  method limits = limits
  method name = "suicide-king"
end
