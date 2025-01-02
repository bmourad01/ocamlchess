open Core_kernel [@@warning "-D"]
open Precalculated_common

let castle_tbl =
  let open Bb in
  let wk = Square.(!!f1 + !!g1) in
  let wq = Square.(!!c1 + !!d1) in
  let bk = Square.(!!f8 + !!g8) in
  let bq = Square.(!!c8 + !!d8) in
  let valid = [
    Piece.White, Cr.Kingside,  wk;
    Piece.White, Cr.Queenside, wq;
    Piece.Black, Cr.Kingside,  bk;
    Piece.Black, Cr.Queenside, bq;
  ] in
  Array.init (1 lsl Cr.bits) ~f:(fun i ->
      let x = Cr.of_int_unsafe i in
      List.fold valid ~init:empty ~f:(fun acc (c, s, b) ->
          if Cr.mem x c s then acc + b else acc))

let[@inline] castle cr c s =
  Cr.(to_int @@ inter cr @@ singleton c s) |> Array.unsafe_get castle_tbl
