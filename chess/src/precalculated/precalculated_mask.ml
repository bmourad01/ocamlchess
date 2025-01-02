(* Masks for various movement directions. *)

open Core_kernel [@@warning "-D"]

(* Direction to move in when starting from a particular square. *)
let dir =
  let n = (Square.count lsr 3) - 1 in
  fun r f -> Precalculated_simple.make @@ fun rank file ->
    List.init n ~f:(fun i -> r rank (i + 1), f file (i + 1))

module Tbl = struct
  (* All 8 directions. *)
  let east  = dir const (+)
  and west  = dir const (-)
  and north = dir (+) const
  and south = dir (-) const
  and neast = dir (+) (+)
  and nwest = dir (+) (-)
  and seast = dir (-) (+)
  and swest = dir (-) (-)

  (* Combine all diagonal directions, minus the edges. *)
  let diagonal = Array.init Square.count ~f:(fun i -> Bitboard.(
      neast.(i) + nwest.(i) + seast.(i) + swest.(i) - edges))

  (* Combine all orthogonal directions, minus the edges. *)
  let orthogonal = Array.init Square.count ~f:(fun i -> Bitboard.(
      (east.(i)  - file_h) +
      (west.(i)  - file_a) +
      (north.(i) - rank_8) +
      (south.(i) - rank_1)))
end

let east  sq = Array.unsafe_get Tbl.east  @@ Square.to_int sq
let west  sq = Array.unsafe_get Tbl.west  @@ Square.to_int sq
let north sq = Array.unsafe_get Tbl.north @@ Square.to_int sq
let south sq = Array.unsafe_get Tbl.south @@ Square.to_int sq
let neast sq = Array.unsafe_get Tbl.neast @@ Square.to_int sq
let nwest sq = Array.unsafe_get Tbl.nwest @@ Square.to_int sq
let seast sq = Array.unsafe_get Tbl.seast @@ Square.to_int sq
let swest sq = Array.unsafe_get Tbl.swest @@ Square.to_int sq
