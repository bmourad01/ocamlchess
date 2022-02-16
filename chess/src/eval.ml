open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

(* Relative material advantage. *)
let material =
  let kinds = Piece.[Pawn; Knight; Bishop; Rook; Queen] in
  fun pos ->
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    List.fold kinds ~init:0 ~f:(fun acc k ->
        let b = Position.board_of_kind pos k in
        let v = Piece.Kind.value k in
        let d = Bb.(count (b & us)) - Bb.(count (b & them)) in
        acc + v * d)

let mobility_bonus = Map.of_alist_exn (module Piece.Kind) [
    Pawn, 0;
    Knight, 4;
    Bishop, 3;
    Rook, 0;
    Queen, 0;
    King, 0;
  ]

let mobility ?(swap = false) =
  let kinds = Piece.[Knight; Bishop; Rook; Queen; King] in
  fun pos ->
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let occupied = Bb.(us + them) in
    let b = if swap then them else us in
    List.fold kinds ~init:0 ~f:(fun acc k ->
        let f = match k with
          | Pawn   -> assert false
          | Knight -> Pre.knight
          | Bishop -> fun sq -> Pre.bishop sq occupied
          | Rook   -> fun sq -> Pre.rook   sq occupied
          | Queen  -> fun sq -> Pre.queen  sq occupied
          | King   -> Pre.king in
        let bonus = Map.find_exn mobility_bonus k in
        Bb.(Position.board_of_kind pos k & b) |>
        Bb.fold ~init:acc ~f:(fun acc sq ->
            acc + (Bb.(count (f sq - b)) * bonus)))

(* Relative mobility advantage. *)
let mobility pos = mobility pos - mobility pos ~swap:true

(* Overall evaluation. *)
let go pos = material pos + mobility pos
