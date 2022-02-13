open Core_kernel

module Bb = Bitboard

let pawn_val = Piece.Kind.value Pawn
let knight_val = Piece.Kind.value Knight
let bishop_val = Piece.Kind.value Bishop
let rook_val = Piece.Kind.value Rook
let queen_val = Piece.Kind.value Queen

let calculate_material pos =
  let active = Position.active_board pos in
  let inactive = Position.inactive_board pos in
  let pawn = Position.pawn pos in
  let knight = Position.knight pos in
  let bishop = Position.bishop pos in
  let rook = Position.rook pos in
  let queen = Position.queen pos in
  (Bb.(count (pawn & active)) - Bb.(count (pawn & inactive))) * pawn_val +
  (Bb.(count (knight & active)) - Bb.(count (knight & inactive))) * knight_val +
  (Bb.(count (bishop & active)) - Bb.(count (bishop & inactive))) * bishop_val +
  (Bb.(count (rook & active)) - Bb.(count (rook & inactive))) * rook_val +
  (Bb.(count (queen & active)) - Bb.(count (queen & inactive))) * queen_val

let go pos = calculate_material pos
