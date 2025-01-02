open Core_kernel [@@warning "-D"]
open Precalculated_common
open Precalculated_piece

let between_tbl =
  let tbl = Array.create Bb.empty ~len:Square.(count * count) in
  for i = 0 to Square.count - 1 do
    let sq = Square.of_int_unsafe i in
    for j = 0 to Square.count - 1 do
      let open Bb in
      (* Use the singleton bitboard of the target square as the
         blocker mask. *)
      let s = !!Square.(of_int_unsafe j) in
      let orthogonal m =
        let b = rook sq s & m in
        if (b & s) <> empty then b - s else empty in
      let diagonal m =
        let b = bishop sq s & m in
        if (b & s) <> empty then b - s else empty in
      (* Use the pre-generated directional masks. *)
      let east  = orthogonal Mask.Tbl.east.(i)  in
      let west  = orthogonal Mask.Tbl.west.(i)  in
      let north = orthogonal Mask.Tbl.north.(i) in
      let south = orthogonal Mask.Tbl.south.(i) in
      let neast = diagonal   Mask.Tbl.neast.(i) in
      let nwest = diagonal   Mask.Tbl.nwest.(i) in
      let seast = diagonal   Mask.Tbl.seast.(i) in
      let swest = diagonal   Mask.Tbl.swest.(i) in
      (* We're getting the union of all directions, even though only
         one of them will be valid. *)
      tbl.(Int.(i + j * Square.count)) <-
        east + west + north + south + neast + nwest + seast + swest
    done
  done;
  tbl

let[@inline] between x y =
  let i = Square.to_int x in
  let j = Square.to_int y in
  Array.unsafe_get between_tbl (i + j * Square.count)
