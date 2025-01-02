open Core_kernel [@@warning "-D"]
open Precalculated_piece

let connected_pawns_idx sq c = (c * Square.count) + sq

let connected_pawns_tbl =
  let len = Square.count * Piece.Color.count in
  let tbl = Array.create Bitboard.empty ~len in
  for i = 8 to (Square.count - 8) - 1 do
    let wc = pawn_capture (Square.of_int_exn i) Piece.White in
    let wf = pawn_capture (Square.of_int_exn (i + 8)) Piece.White in
    let bc = pawn_capture (Square.of_int_exn i) Piece.Black in
    let bf = pawn_capture (Square.of_int_exn (i - 8)) Piece.Black in
    tbl.(connected_pawns_idx i Piece.Color.white) <- Bitboard.(bc + bf);
    tbl.(connected_pawns_idx i Piece.Color.black) <- Bitboard.(wc + wf);
  done;
  tbl

let[@inline] connected_pawns sq c =
  let sq = Square.to_int sq in
  let c = Piece.Color.to_int c in
  Array.unsafe_get connected_pawns_tbl @@
  connected_pawns_idx sq c
