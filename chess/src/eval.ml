open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let material_weight = 100

let endgame_material =
  Float.of_int (material_weight * begin
      Piece.Kind.value Rook * 2 +
      Piece.Kind.value Bishop +
      Piece.Kind.value Knight
    end)

(* Weigh a score based on whether we're in endgame phase or not. *)
let weigh_start n endgame = Float.(to_int (of_int n * (1.0 - endgame)))
let weigh_end n endgame = Float.(to_int (of_int n * endgame))

(* Weighted sum of pawns. *)
let pawns =
  fun ?(swap = false) pos ->
  let us = Position.active_board pos in
  let them = Position.inactive_board pos in
  let b = if swap then them else us in
  let b' = Position.board_of_kind pos Pawn in
  let v = Piece.Kind.value Pawn in
  v * Bb.(count (b & b')) * material_weight

(* Weighted sum of material (minus pawns). *)
let material =
  let kinds = Piece.[Knight; Bishop; Rook; Queen] in
  fun ?(swap = false) pos ->
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let b = if swap then them else us in
    List.fold kinds ~init:0 ~f:(fun acc k ->
        let b' = Position.board_of_kind pos k in
        let v = Piece.Kind.value k in
        acc + v * Bb.(count (b & b')) * material_weight)

(* Weighted sum of the "mobility" of the material. *)
let mobility =
  (* In order: pawn, knight, bishop, rook, queen, king. *)
  let start_bonus = [|0; 4; 3; 0; 0; 0|] in
  let end_bonus   = [|1; 6; 2; 1; 1; 1|] in
  let kinds = Piece.[Knight; Bishop; Rook; Queen; King] in
  fun ?(swap = false) pos endgame ->
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let occupied = Bb.(us + them) in
    let b = if swap then them else us in
    let pawn =
      (* For pawns, we not only want to evaluate attacked squares but
         also the ability to push to new ranks. *)
      let pawn = Position.pawn pos in
      let get f r fl fr =
        (* Check the next rank (shift by 8). *)
        let n = Bb.to_int64 pawn in
        let single = Bb.((of_int64 Int64.(f n 8)) - occupied) in
        (* If that rank is OK, then check the rank after that. *)
        let n = Bb.(to_int64 (single & r)) in
        let double = Bb.((of_int64 Int64.(f n 8)) - occupied) in
        (* Check that we can attack the files to the left and right. *)
        let n = Bb.to_int64 pawn in
        let att = Bb.((of_int64 (f n 9) - fl) + (of_int64 (f n 7) - fr)) in
        single, double, att in
      let single, double, att = match Position.active pos with
        | Piece.White -> get Int64.(lsl) Bb.rank_3 Bb.file_a Bb.file_h
        | Piece.Black -> get Int64.(lsr) Bb.rank_6 Bb.file_h Bb.file_a in
      let attackable = Bb.(them - Position.king pos) in
      let n = Bb.(count (single + double + (att & attackable))) in
      let i = Piece.Kind.pawn in
      let start_bonus = Array.unsafe_get start_bonus i in
      let end_bonus = Array.unsafe_get end_bonus i in
      let start = weigh_start (n * start_bonus) endgame in
      let end_ = weigh_end (n * end_bonus) endgame in
      start + end_ in
    let mobility =
      (* For the rest, count the number of attacked squares. *)
      List.fold kinds ~init:0 ~f:(fun acc k ->
          let f = match k with
            | Pawn   -> assert false
            | Knight -> Pre.knight
            | Bishop -> fun sq -> Pre.bishop sq occupied
            | Rook   -> fun sq -> Pre.rook   sq occupied
            | Queen  -> fun sq -> Pre.queen  sq occupied
            | King   -> Pre.king in
          let i = Piece.Kind.to_int k in
          let start_bonus = Array.unsafe_get start_bonus i in
          let end_bonus = Array.unsafe_get end_bonus i in
          Bb.(Position.board_of_kind pos k & b) |>
          Bb.fold ~init:acc ~f:(fun acc sq ->
              let n = Bb.(count (f sq - b)) in
              let start = weigh_start (n * start_bonus) endgame in
              let end_ = weigh_end (n * end_bonus) endgame in
              acc + start + end_)) in
    mobility + pawn

(* Relative mobility advantage. *)
let mobility pos our_endgame their_endgame =
  mobility pos our_endgame - mobility pos their_endgame ~swap:true

(* Count the rooks on open files. This can also be measured by the mobility
   score, but we also give a bonus here. *)
let rook_open_file ?(swap = false) pos =
  let us = Position.active_board pos in
  let them = Position.inactive_board pos in
  let occupied = Bb.(us + them) in
  let b = if swap then them else us in
  let rook = Bb.(b & Position.rook pos) in
  List.init Square.File.count ~f:ident |>
  List.fold ~init:0 ~f:(fun acc i ->
      let f = Bb.file_exn i in
      let b = Bb.(f & rook) in
      if Bb.(b <> empty && b = (f & occupied))
      then acc + 1 else acc)

(* Relative advantage of rooks on open files. *)
let rook_open_file pos = rook_open_file pos - rook_open_file pos ~swap:true

(* Weighted sum of piece placement (using piece-square tables). *)
let placement =
  let pawn = [|
    0;  0;   0;   0;   0;   0;   0;  0;
    50; 50;  50;  50;  50;  50;  50; 50;
    10; 10;  20;  30;  30;  20;  10; 10;
    5;  5;   10;  25;  25;  10;  5;  5;
    0;  0;   0;   20;  20;  0;   0;  0;
    5; -5;  -10;  0;   0;  -10; -5;  5;
    5;  10;  10; -20; -20;  10;  10; 5;
    0;  0;   0;   0;   0;   0;   0;  0;
  |] in
  let knight = [|
    -50; -40; -30; -30; -30; -30; -40; -50;
    -40; -20;  0;   0;   0;   0;  -20; -40;
    -30;  0;   10;  15;  15;  10;  0;  -30;
    -30;  5;   15;  20;  20;  15;  5;  -30;
    -30;  5;   10;  15;  15;  10;  0;  -30;
    -30;  5;   15;  20;  20;  15;  5;  -30;
    -40; -20;  0;   5;   5;   0;  -20; -40;
    -50; -40; -30; -30; -30; -30; -40; -50;
  |] in
  let bishop = [|
    -20; -10; -10; -10; -10; -10; -10; -20;
    -10;  0;   0;   0;   0;   0;   0;  -10;
    -10;  0;   5;   10;  10;  5;   0;  -10;
    -10;  5;   5;   10;  10;  5;   5;  -10;
    -10;  0;   10;  10;  10;  10;  0;  -10;
    -10;  10;  10;  10;  10;  10;  10; -10;
    -10;  5;   0;   0;   0;   0;   5;  -10;
    -20; -10; -10; -10; -10; -10; -10; -20;
  |] in
  let rook = [|
    +0; 0;  0;  0;  0;  0;  0;  0;
    +5; 10; 10; 10; 10; 10; 10; 5;
    -5; 0;  0;  0;  0;  0;  0; -5;
    -5; 0;  0;  0;  0;  0;  0; -5;
    -5; 0;  0;  0;  0;  0;  0; -5;
    -5; 0;  0;  0;  0;  0;  0; -5;
    -5; 0;  0;  0;  0;  0;  0; -5;
    +0; 0;  0;  0;  5;  5;  0;  0;
  |] in
  let queen = [|
    -20; -10; -10; -5; -5; -10; -10; -20;
    -10;  0;   0;   0;  0;  0;   0;   0;
    -10;  0;   5;   5;  5;  5;   0;  -10;
    -5;   0;   5;   5;  5;  5;   0;  -5;
    +0;   0;   5;   5;  5;  5;   0;  -5;
    -10;  5;   5;   5;  5;  5;   5;  -10;
    -10;  0;   5;   0;  0;  0;   0;  -10;
    -20; -10; -10; -5; -5; -10; -10; -20;
  |] in
  let king_start = [|
    -30; -40; -40; -50; -50; -40; -40; -30;
    -30; -40; -40; -50; -50; -40; -40; -30;
    -30; -40; -40; -50; -50; -40; -40; -30;
    -30; -40; -40; -50; -50; -40; -40; -30;
    -20; -30; -30; -40; -40; -30; -30; -20;
    -10; -20; -20; -20; -20; -20; -20; -10;
    +20;  20;  0;   0;   0;   0;   20;  20;
    +20;  30;  10;  0;   0;   10;  20;  30;
  |] in
  let king_end = [|
    -50; -40; -30; -20; -20; -30; -40; -50;
    -30; -20; -10;  0;   0;  -10; -20; -30;
    -30; -10;  20;  30;  30;  20; -10; -30;
    -30; -10;  30;  40;  40;  30; -10; -30;
    -30; -10;  30;  40;  40;  30; -10; -30;
    -30; -10;  20;  30;  30;  20; -10; -30;
    -30; -30;  0;   0;   0;   0;  -30; -30;
    -50; -30; -30; -30; -30; -30; -30; -50;
  |] in
  let get t sq c =
    let sq = match c with
      | Piece.White -> Square.(with_rank_exn sq (7 - rank sq))
      | Piece.Black -> sq in
    Array.unsafe_get t @@ Square.to_int sq in
  fun ?(swap = false) pos endgame ->
    let c = if swap then Position.inactive pos else Position.active pos in
    Position.collect_color pos c |> List.fold ~init:0 ~f:(fun acc (sq, k) ->
        let v = match k with
          | Piece.Pawn   -> weigh_start (get pawn   sq c) endgame
          | Piece.Knight -> weigh_start (get knight sq c) endgame
          | Piece.Bishop -> weigh_start (get bishop sq c) endgame
          | Piece.Rook   -> weigh_start (get rook   sq c) endgame
          | Piece.Queen  -> weigh_start (get queen  sq c) endgame
          | Piece.King   ->
            weigh_start (get king_start sq c) endgame +
            weigh_end   (get king_end   sq c) endgame in
        acc + v)

(* Relative placement advantage. *)
let placement pos our_endgame their_endgame =
  placement pos our_endgame - placement pos their_endgame ~swap:true

(* Mop-up score (for endgames).

   See: https://www.chessprogramming.org/Mop-up_Evaluation
*)
let mop_up ?(swap = false) pos endgame =
  let us = Position.active_board pos in
  let them = Position.inactive_board pos in
  let b, b' = if swap then them, us else us, them in
  let our_king = Bb.(first_set_exn (b & Position.king pos)) in
  let their_king = Bb.(first_set_exn (b' & Position.king pos)) in
  let n =
    Square.manhattan_center their_king * 10 +
    (14 - Square.manhattan our_king their_king) * 4 in
  weigh_end n endgame

(* Calculate the endgame weight based on the amount of material on the
   board. *)
let endgame_weight material =
  let m = 1.0 /. endgame_material in
  1.0 -. Float.(min 1.0 (of_int material * m))

(* Overall evaluation. *)
let go pos =
  let our_pawns = pawns pos in
  let their_pawns = pawns pos ~swap:true in
  let our_material = material pos in
  let their_material = material pos ~swap:true in
  let our_endgame = endgame_weight our_material in
  let their_endgame = endgame_weight their_material in
  let material = (our_material + our_pawns) - (their_material + their_pawns) in
  let our_mop_up =
    if our_material > their_material + Piece.Kind.value Pawn * 2
    && Float.(our_endgame > 0.0)
    then mop_up pos our_endgame
    else 0 in
  let their_mop_up =
    if their_material > our_material + Piece.Kind.value Pawn * 2
    && Float.(their_endgame > 0.0)
    then mop_up pos their_endgame ~swap:true
    else 0 in
  material +
  mobility pos our_endgame their_endgame +
  rook_open_file pos +
  placement pos our_endgame their_endgame +
  (our_mop_up - their_mop_up)
