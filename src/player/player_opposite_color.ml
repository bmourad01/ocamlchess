open Core_kernel

module Bb = Bitboard

let opposite_color active sq =
  let rank, file = Square.decomp sq in
  match (active : Piece.color) with
  | White -> (rank land 1) = (file land 1)
  | Black -> (rank land 1) <> (file land 1)

let choose pos = match Position.legal_moves pos with
  | [] -> raise Player.No_moves
  | moves ->
    let active = Position.active pos in
    Player.equal_eval moves ~eval:(fun mv ->
        let pos = Position.Legal_move.position mv in
        Option.return @@ Bb.count @@
        Bb.filter ~f:(opposite_color active) @@
        Position.board_of_color pos active) |>
    List.random_element_exn

let create ?(limits = None) () = Player.create ~choose ~limits ()
