open Core_kernel

module Bb = Bitboard
module Lms = Position.Legal_moves

let opposite_color active sq =
  let rank, file = Square.decomp sq in
  match (active : Piece.color) with
  | White -> (rank land 1) = (file land 1)
  | Black -> (rank land 1) <> (file land 1)

let choose lms = match Lms.decomp lms with
  | [], _ -> raise Player.No_moves
  | moves, pos ->
    let active = Position.active pos in
    Player.best_moves moves ~eval:(fun mv ->
        let pos = Position.Legal_move.position mv in
        Option.return @@ Bb.count @@
        Bb.filter ~f:(opposite_color active) @@
        Position.board_of_color pos active) |>
    List.random_element_exn

let create ?(limits = None) () = Player.create ~choose ~limits ()