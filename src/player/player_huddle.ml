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
    Player.best_moves moves ~eval:(fun mv ->
        let pos = Legal.position mv in
        let active_board = Position.board_of_color pos active in
        let king = Position.king pos in
        let king_sq = Bb.(first_set_exn (king & active_board)) in
        Position.find_color pos active |>
        List.fold ~init:0 ~f:(fun acc (sq, (k : Piece.kind)) ->
            (* Ignore our king. *)
            match k with
            | King -> acc
            | _ -> acc - chebyshev king_sq sq) |>
        Option.return) |>
    List.random_element_exn

let create ?(limits = None) () = Player.create ~choose ~limits ()
