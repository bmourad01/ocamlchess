(* Simple movement patterns which can be wholly precalculated with no
   parameters. *)

open Core_kernel [@@warning "-D"]

(* Construct a simple table which maps squares to bitboards. *)
let make f = Array.init Square.count ~f:(fun i ->
    let open Square in
    let sq = of_int_unsafe i in
    f (rank sq) (file sq) |>
    List.filter_map ~f:(fun (rank, file) -> create ~rank ~file) |>
    List.fold ~init:Bitboard.empty ~f:Bitboard.set)

let white_pawn_advance = make @@ fun rank file -> [rank + 1, file]
let black_pawn_advance = make @@ fun rank file -> [rank - 1, file]

let white_pawn_capture = make @@ fun rank file -> [
    rank + 1, file + 1;
    rank + 1, file - 1;
  ]

let black_pawn_capture = make @@ fun rank file -> [
    rank - 1, file + 1;
    rank - 1, file - 1;
  ]

let pawn_advance = Array.append white_pawn_advance black_pawn_advance
let pawn_capture = Array.append white_pawn_capture black_pawn_capture

let knight = make @@ fun rank file -> [
    rank + 2, file + 1;
    rank - 2, file + 1;
    rank + 2, file - 1;
    rank - 2, file - 1;
    rank + 1, file + 2;
    rank - 1, file + 2;
    rank + 1, file - 2;
    rank - 1, file - 2;
  ]

let king = make @@ fun rank file -> [
    rank,     file + 1;
    rank,     file - 1;
    rank + 1, file + 1;
    rank + 1, file - 1;
    rank - 1, file + 1;
    rank - 1, file - 1;
    rank + 1, file;
    rank - 1, file;
  ]
