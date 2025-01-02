open Core_kernel [@@warning "-D"]
open Precalculated_common

let passed_pawns_tbl =
  let idx c sq = (c * Square.count) + sq in
  let tbl =
    let len = Piece.Color.count * Square.count in
    Array.create ~len Bb.empty in
  for i = 0 to Square.count - 1 do
    let open Bb in
    let sq = Square.of_int_exn i in
    let n = Mask.north sq in
    let s = Mask.south sq in
    let a, h = file_a, file_h in
    let w = n + ((n << 1) - a) + ((n >> 1) - h) in
    let b = s + ((s >> 1) - h) + ((s << 1) - a) in
    tbl.(idx Piece.Color.white i) <- w;
    tbl.(idx Piece.Color.black i) <- b;
  done;
  tbl

let[@inline] passed_pawns sq c =
  let s = Square.to_int sq in
  let c = Piece.Color.to_int c in
  let i = (c * Square.count) + s in
  Array.unsafe_get passed_pawns_tbl i
