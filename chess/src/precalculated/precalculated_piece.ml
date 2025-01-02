open Core_kernel [@@warning "-D"]
open Precalculated_common

module Sliding = Precalculated_sliding

let[@inline] pawn_advance sq c =
  let i = Square.to_int sq + Piece.Color.to_int c * Square.count in
  Array.unsafe_get Simple.pawn_advance i

let[@inline] pawn_capture sq c =
  let i = Square.to_int sq + Piece.Color.to_int c * Square.count in
  Array.unsafe_get Simple.pawn_capture i

let[@inline] knight sq = Array.unsafe_get Simple.knight @@ Square.to_int sq

let[@inline] bishop sq occupied =
  let i = Square.to_int sq in
  let mask = Array.unsafe_get Mask.Tbl.diagonal i in
  let shift = Array.unsafe_get Shift.diagonal' i in
  let magic = Array.unsafe_get Magic.bishop i in
  let occupied = Bb.(to_int64 (occupied & mask)) in
  let hash = Sliding.hash occupied magic shift in
  Array.unsafe_get Sliding.bishop (i + hash * Square.count)

let[@inline] rook sq occupied =
  let i = Square.to_int sq in
  let mask = Array.unsafe_get Mask.Tbl.orthogonal i in
  let shift = Array.unsafe_get Shift.orthogonal' i in
  let magic = Array.unsafe_get Magic.rook i in
  let occupied = Bb.(to_int64 (occupied & mask)) in
  let hash = Sliding.hash occupied magic shift in
  Array.unsafe_get Sliding.rook (i + hash * Square.count)

let[@inline] queen sq occupied = Bb.(bishop sq occupied + rook sq occupied)
let[@inline] king sq = Array.unsafe_get Simple.king @@ Square.to_int sq

let[@inline] attacks sq occupied c : Piece.kind -> Bb.t = function
  | Pawn   -> pawn_capture sq c
  | Knight -> knight sq
  | Bishop -> bishop sq occupied
  | Rook   -> rook sq occupied
  | Queen  -> queen sq occupied
  | King   -> king sq
