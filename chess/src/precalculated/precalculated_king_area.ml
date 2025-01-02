open Core_kernel [@@warning "-D"]
open Precalculated_common
open Precalculated_piece

let king_area_tbl =
  let tbl = Array.create ~len:(Piece.Color.count * Square.count) Bb.empty in
  for c = 0 to Piece.Color.count - 1 do
    for s = 0 to Square.last do
      let open Bb.Syntax in
      let sq = Square.of_int_exn s in
      let att = king sq in
      let up = match Piece.Color.of_int_exn c with
        | Piece.White -> att << 8
        | Piece.Black -> att >> 8 in
      let m = (att + up) ++ sq in
      let m = match Square.file sq with
        | 0 -> m + (m << 1)
        | 7 -> m + (m >> 1)
        | _ -> m in
      let i = Int.((c * Square.count) + s) in
      tbl.(i) <- m
    done;
  done;
  tbl

let[@inline] king_area sq c =
  let s = Square.to_int sq in
  let c = Piece.Color.to_int c in
  let i = (c * Square.count) + s in
  Array.unsafe_get king_area_tbl i
