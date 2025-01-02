open Core_kernel [@@warning "-D"]

let forward_ranks_idx c r = (c * Square.Rank.count) + r

let forward_ranks_tbl =
  let len = Square.Rank.count * Piece.Color.count in
  let t = Array.create ~len Bitboard.empty in
  for r = 0 to Square.Rank.count - 1 do
    let i = forward_ranks_idx Piece.Color.black r in
    let j = forward_ranks_idx Piece.Color.white r in
    for i = r to Square.Rank.count - 1 do
      t.(j) <- Bitboard.(t.(j) + rank_exn i);
    done;
    t.(i) <- Bitboard.(~~(t.(j)) + rank_exn r);
  done;
  t

let[@inline] forward_ranks sq c =
  let c = Piece.Color.to_int c in
  Array.unsafe_get forward_ranks_tbl @@
  forward_ranks_idx c @@
  Square.rank sq
