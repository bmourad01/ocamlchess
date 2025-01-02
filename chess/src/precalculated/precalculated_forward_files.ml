open Core_kernel [@@warning "-D"]
open Precalculated_forward_ranks

let forward_files_idx c sq = (c * Square.count) + sq

let forward_files_tbl =
  let len = Square.count * Piece.Color.count in
  let t = Array.create ~len Bitboard.empty in
  for c = 0 to Piece.Color.count - 1 do
    for s = 0 to Square.count - 1 do
      let sq = Square.of_int_exn s in
      let r = Square.rank sq in
      let ri = forward_ranks_idx c r in
      let f = Bitboard.file_exn @@ Square.file sq in
      t.(forward_files_idx c s) <- Bitboard.(f & forward_ranks_tbl.(ri));
    done;
  done;
  t

let[@inline] forward_files sq c =
  let c = Piece.Color.to_int c in
  let s = Square.to_int sq in
  Array.unsafe_get forward_files_tbl @@
  forward_files_idx c s
