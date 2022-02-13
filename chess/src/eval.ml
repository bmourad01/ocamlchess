open Core_kernel

module Bb = Bitboard

let calculate_material =
  let kinds = Piece.[Pawn; Knight; Bishop; Rook; Queen] in
  fun pos ->
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    List.fold kinds ~init:0 ~f:(fun acc k ->
        let b = Position.board_of_kind pos k in
        let v = Piece.Kind.value k in
        acc + (Bb.(count (b & us)) - Bb.(count (b & them))) * v)

let go pos = calculate_material pos
