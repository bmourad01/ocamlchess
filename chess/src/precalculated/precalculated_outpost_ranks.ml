open Core_kernel [@@warning "-D"]

let outpost_ranks_tbl = Bitboard.[|
    rank_4 + rank_5 + rank_6;
    rank_3 + rank_4 + rank_5;
  |]

let[@inline] outpost_ranks c =
  Array.unsafe_get outpost_ranks_tbl @@ Piece.Color.to_int c
