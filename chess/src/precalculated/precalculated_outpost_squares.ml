open Core_kernel [@@warning "-D"]
open Precalculated_passed_pawns

let outpost_squares_idx sq c = (c * Square.count) + sq

let outpost_squares_tbl =
  let len = Square.count * Piece.Color.count in
  let tbl = Array.create Bitboard.empty ~len in
  for c = 0 to Piece.Color.count - 1 do
    for s = 0 to Square.last do
      let sq = Square.of_int_exn s in
      let p = passed_pawns sq @@ Piece.Color.of_int_exn c in
      let f = Bitboard.file_exn @@ Square.file sq in
      tbl.(outpost_squares_idx s c) <- Bitboard.(p - f);
    done;
  done;
  tbl

let[@inline] outpost_squares sq c =
  let s = Square.to_int sq in
  let c = Piece.Color.to_int c in
  Array.unsafe_get outpost_squares_tbl @@
  outpost_squares_idx s c
