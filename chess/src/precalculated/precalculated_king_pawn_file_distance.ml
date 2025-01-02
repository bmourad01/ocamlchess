open Core_kernel [@@warning "-D"]
open Precalculated_common

let king_pawn_file_distance_idx f m =
  let m = Int64.to_int_trunc m in
  m + (f * (1 lsl Square.File.count))

let king_pawn_file_distance_tbl =
  let fc = Square.File.count in
  let fc' = fc - 1 in
  let len = fc * (1 lsl fc) in
  let t = Array.create ~len 0 in
  for m = 0 to 0xFF do
    for f = 0 to fc' do
      let open Int64 in
      let m = of_int m in
      let sh = Int.(fc - f - 1) in
      let l = (0xFFL land (m lsl sh)) lsr sh in
      let r = (m lsr f) lsl f in
      let ldist = if l = 0L then fc' else Int.(f - msb l) in
      let rdist = if r = 0L then fc' else Int.(lsb r - f) in
      let dist = if (l lor r) = 0L then 0 else Int.min ldist rdist in
      t.(king_pawn_file_distance_idx f m) <- dist;
    done;
  done;
  t

let[@inline] king_pawn_file_distance sq pawn =
  let open Int64 in
  let f = Square.file sq in
  let p = Bb.to_int64 pawn in
  let p = p lor (p lsr 8) in
  let p = p lor (p lsr 16) in
  let p = p lor (p lsr 32) in
  let i = king_pawn_file_distance_idx f (p land 0xFFL) in
  Array.unsafe_get king_pawn_file_distance_tbl i
