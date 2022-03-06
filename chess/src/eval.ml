open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let material_weight = 100

(* Threshold value that will keep the endgame weight very low
   until some significant amount material is lost. *)
let endgame_scale =
  let material =
    Piece.Kind.value Rook * 2 +
    Piece.Kind.value Bishop +
    Piece.Kind.value Knight in
  Float.(1.0 / of_int Int.(material_weight * material))

(* Margin to measure whether a mop-up evaluation matters. *)
let pawn_margin = Piece.Kind.value Pawn * 2 * material_weight

(* Weigh a score based on whether we're in endgame phase or not. *)
let weigh_start n endgame = Float.(to_int (of_int n * (1.0 - endgame)))
let weigh_end n endgame = Float.(to_int (of_int n * endgame))

module Material = struct
  (* Weighted sum of pawns. *)
  let pawns ?(swap = false) pos =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let b = if swap then them else us in
    let b' = Position.board_of_kind pos Pawn in
    let v = Piece.Kind.value Pawn in
    v * Bb.(count (b & b')) * material_weight

  (* Weighted sum of material (minus pawns). *)
  let not_pawns =
    let kinds = Piece.[Knight; Bishop; Rook; Queen] in
    fun ?(swap = false) pos ->
      let us = Position.active_board pos in
      let them = Position.inactive_board pos in
      let b = if swap then them else us in
      List.fold kinds ~init:0 ~f:(fun acc k ->
          let b' = Position.board_of_kind pos k in
          let v = Piece.Kind.value k in
          acc + v * Bb.(count (b & b')) * material_weight)

  (* Calculate the endgame weight based on the amount of material on
     the board. *)
  let endgame_weight material =
    Float.(1.0 - (min 1.0 (of_int material * endgame_scale)))
end

module Mobility = struct
  (* In order: pawn, knight, bishop, rook, queen, king. *)
  let start_bonus = [|0; 4; 3; 0; 0; 0|]
  let end_bonus   = [|1; 6; 2; 1; 1; 1|]
  let kinds       = Piece.[Knight; Bishop; Rook; Queen; King]

  (* Weighted sum of the "mobility" of the material. *)
  let go ?(swap = false) pos endgame =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let occupied = Bb.(us + them) in
    let b = if swap then them else us in
    let c = if swap then Position.inactive pos else Position.active pos in
    let pawn =
      (* For pawns, we not only want to evaluate attacked squares but
         also the ability to push to new ranks. *)
      let pawn = Bb.(b & Position.pawn pos) in
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
      let single, double, att = match c with
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
  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module Rook_open_file = struct
  let start_bonus = 20
  let end_bonus = 40

  (* Count the rooks on open files. This can also be measured by the mobility
     score, but we also give a bonus here. *)
  let go ?(swap = false) pos endgame =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let occupied = Bb.(us + them) in
    let b = if swap then them else us in
    let rook = Bb.(b & Position.rook pos) in
    let score =
      List.init Square.File.count ~f:ident |>
      List.fold ~init:0 ~f:(fun acc i ->
          let f = Bb.file_exn i in
          let b = Bb.(f & rook) in
          if Bb.(b <> empty && b = (f & occupied))
          then acc + 1 else acc) in
    weigh_start (score * start_bonus) endgame +
    weigh_end   (score * end_bonus)   endgame

  (* Relative advantage of rooks on open files. *)
  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module Bishop_pair = struct
  let start_bonus = 45
  let end_bonus = 55

  let go ?(swap = false) pos endgame =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let b = if swap then them else us in
    let bishop = Bb.(b & Position.bishop pos) in
    let has_pair =
      Bb.(count (bishop & black)) <> 0 &&
      Bb.(count (bishop & white)) <> 0 in
    let score = Bool.to_int has_pair in
    weigh_start (score * start_bonus) endgame +
    weigh_end   (score * end_bonus)   endgame

  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module King_pawn_shield = struct
  let bonus = 10

  (* Given the king's square for a particular side, calculate the mask
     for pawns that can shield it (left, right, and center) from their
     starting ranks. *)
  let masks =
    let wf, wfl, wfr, wr = Int64.(lsl), Bb.file_a, Bb.file_h, Bb.rank_2 in
    let bf, bfl, bfr, br = Int64.(lsr), Bb.file_h, Bb.file_a, Bb.rank_7 in
    let tbl = Array.create ~len:(Piece.Color.count * Square.count) Bb.empty in
    for i = 0 to Piece.Color.count - 1 do
      for j = 0 to Square.count - 1 do
        let sq = Int64.(1L lsl j) in
        tbl.(Piece.Color.white + j * Piece.Color.count) <-
          Bb.(((of_int64 (wf sq 8)) +
               (of_int64 (wf sq 7) - wfr) +
               (of_int64 (wf sq 9) - wfl)) & wr);
        tbl.(Piece.Color.black + j * Piece.Color.count) <-
          Bb.(((of_int64 (bf sq 8)) +
               (of_int64 (bf sq 7) - bfr) +
               (of_int64 (bf sq 9) - bfl)) & br);
      done;
    done;
    tbl

  (* In the opening phase, the king should be protected behind a group
     of pawns. *)
  let go ?(swap = false) pos endgame =
    let us = Position.active pos in
    let them = Position.inactive pos in
    let c = if swap then them else us in
    let b = Position.board_of_color pos c in
    let king = Bb.(b & Position.king pos) in
    let pawn = Bb.(b & Position.pawn pos) in
    let king_sq = Bb.first_set_exn king in
    let m =
      Array.unsafe_get masks
        Piece.Color.(to_int c + Square.to_int king_sq * count) in
    let score = Bb.(count (m & pawn)) in
    weigh_start (score * bonus) endgame

  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

(* Give a bonus to having passed pawns, especially in endgame positions. *)
module Passed_pawns = struct
  let start_bonus = 10
  let end_bonus = 70

  let masks =
    let east b =
      let b' = Bb.to_int64 b in
      Bb.(of_int64 Int64.(b' lsl 1) - file_a) in
    let west b =
      let b' = Bb.to_int64 b in
      Bb.(of_int64 Int64.(b' lsr 1) - file_h) in
    let wf, wl, wr = Precalculated.Mask.north, east, west in
    let bf, bl, br = Precalculated.Mask.south, west, east in
    let tbl = Array.create ~len:(Piece.Color.count * Square.count) Bb.empty in
    for i = 0 to Piece.Color.count - 1 do
      for j = 0 to Square.count - 1 do
        let sq = Square.of_int_exn j in
        let w = wf sq in
        let b = bf sq in
        tbl.(Piece.Color.white + j * Piece.Color.count) <- Bb.(w + wl w + wr w);
        tbl.(Piece.Color.black + j * Piece.Color.count) <- Bb.(w + bl b + br b);
      done;
    done;
    tbl

  let go ?(swap = false) pos endgame =
    let us = Position.active pos in
    let them = Position.inactive pos in
    let c = if swap then them else us in
    let c' = Piece.Color.opposite c in
    let b = Position.board_of_color pos c in
    let b' = Position.board_of_color pos c' in
    let pawn = Bb.(b & Position.pawn pos) in
    let pawn' = Bb.(b' & Position.pawn pos) in
    let rec loop acc b =
      if Bb.(b = empty) then acc
      else
        let sq = Bb.first_set_exn b in
        let m =
          Array.unsafe_get masks
            Piece.Color.(to_int c + Square.to_int sq * count) in
        let acc = acc + Bool.to_int Bb.((m & pawn') = empty) in
        loop acc @@ Bb.(b - (file_exn @@ Square.file sq)) in
    let score = loop 0 pawn in
    weigh_start (score * start_bonus) endgame +
    weigh_end   (score * end_bonus)   endgame

  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

(* Penalize having more than one pawn of the same color on the same file. *)
module Doubled_pawns = struct
  let start_penalty = -20
  let end_penalty = -30

  let go ?(swap = false) pos endgame =
    let us = Position.active pos in
    let them = Position.inactive pos in
    let c = if swap then them else us in
    let b = Position.board_of_color pos c in
    let pawn = Bb.(b & Position.pawn pos) in
    let score =
      List.init Square.File.count ~f:ident |>
      List.fold ~init:0 ~f:(fun acc i ->
          let n = Bb.(count (file_exn i & pawn)) in
          acc + max 0 (n - 1)) in
    weigh_start (score * start_penalty) endgame +
    weigh_end   (score * end_penalty)   endgame

  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module Isolated_pawns = struct
  let start_penalty = -15
  let end_penalty = -30

  let neighbor_files = Bb.[|
      file_b;
      file_a + file_c;
      file_b + file_d;
      file_c + file_e;
      file_d + file_f;
      file_e + file_g;
      file_f + file_h;
      file_g;
    |]

  let go ?(swap = false) pos endgame =
    let us = Position.active pos in
    let them = Position.inactive pos in
    let c = if swap then them else us in
    let b = Position.board_of_color pos c in
    let pawn = Bb.(b & Position.pawn pos) in
    let score =
      List.init Square.File.count ~f:ident |>
      List.fold ~init:0 ~f:(fun acc i ->
          let f = Bb.file_exn i in
          let nf = Array.unsafe_get neighbor_files i in
          let isolated = Bb.(((f & pawn) <> empty) && ((nf & pawn) = empty)) in
          acc + Bool.to_int isolated) in
    weigh_start (score * start_penalty) endgame +
    weigh_end   (score * end_penalty)   endgame

  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module Placement = struct
  (* Piece-square tables. *)
  module Tables = struct
    let pawn_start = [|
      0;  0;   0;   0;   0;   0;   0;  0;
      50; 50;  50;  50;  50;  50;  50; 50;
      10; 10;  20;  30;  30;  20;  10; 10;
      5;  5;   10;  25;  25;  10;  5;  5;
      0;  0;   0;   20;  20;  0;   0;  0;
      5; -5;  -10;  0;   0;  -10; -5;  5;
      5;  10;  10; -20; -20;  10;  10; 5;
      0;  0;   0;   0;   0;   0;   0;  0;
    |]

    let pawn_end = [|
      +0;   0;   0;   0;   0;   0;   0;   0;
      +80;  80;  80;  80;  80;  80;  80;  80;
      +60;  60;  60;  60;  60;  60;  60;  60;
      +40;  40;  40;  40;  40;  40;  40;  40;
      +20;  20;  20;  20;  20;  20;  20;  20;
      +0;   0;   0;   0;   0;   0;   0;   0;
      -20; -20; -20; -20; -20; -20; -20; -20;
      +0;   0;   0;   0;   0;   0;   0;   0;
    |]

    let knight_start = [|
      -50; -40; -30; -30; -30; -30; -40; -50;
      -40; -20;  0;   0;   0;   0;  -20; -40;
      -30;  0;   10;  15;  15;  10;  0;  -30;
      -30;  5;   15;  20;  20;  15;  5;  -30;
      -30;  5;   10;  15;  15;  10;  0;  -30;
      -30;  5;   15;  20;  20;  15;  5;  -30;
      -40; -20;  0;   5;   5;   0;  -20; -40;
      -50; -40; -30; -30; -30; -30; -40; -50;
    |]

    let knight_end = [|
      -50; -40; -30; -30; -30; -30; -40; -50;
      -40; -20;  0;   0;   0;   0;  -20; -40;
      -30;  0;   10;  15;  15;  10;  0;  -30;
      -30;  5;   15;  20;  20;  15;  5;  -30;
      -30;  0;   15;  20;  20;  15;  0;  -30;
      -30;  5;   10;  15;  15;  10;  5;  -30;
      -40; -20;  0;   5;   5;   0;  -20; -40;
      -50; -40; -30; -30; -30; -30; -40; -50;
    |]

    let bishop_start = [|
      -20; -10; -10; -10; -10; -10; -10; -20;
      -10;  0;   0;   0;   0;   0;   0;  -10;
      -10;  0;   5;   10;  10;  5;   0;  -10;
      -10;  5;   5;   10;  10;  5;   5;  -10;
      -10;  0;   10;  10;  10;  10;  0;  -10;
      -10;  10;  10;  10;  10;  10;  10; -10;
      -10;  5;   0;   0;   0;   0;   5;  -10;
      -20; -10; -10; -10; -10; -10; -10; -20;
    |]

    let bishop_end = [|
      -20; -10; -10; -10; -10; -10; -10; -20;
      -10;  0;    0;   0;   0;   0;   0; -10;
      -10;  0;    5;  10;  10;   5;   0; -10;
      -10;  5;    5;  10;  10;   5;   5; -10;
      -10;  0;   10;  10;  10;  10;   0; -10;
      -10;  10;  10;  10;  10;  10;  10; -10;
      -10;  5;    0;   0;   0;   0;   5; -10;
      -20; -10; -10; -10; -10; -10; -10; -20;
    |]

    let rook_start = [|
      +0; 0;  0;  0;  0;  0;  0;  0;
      +5; 10; 10; 10; 10; 10; 10; 5;
      -5; 0;  0;  0;  0;  0;  0; -5;
      -5; 0;  0;  0;  0;  0;  0; -5;
      -5; 0;  0;  0;  0;  0;  0; -5;
      -5; 0;  0;  0;  0;  0;  0; -5;
      -5; 0;  0;  0;  0;  0;  0; -5;
      +0; 0;  0;  0;  5;  5;  0;  0;
    |]

    let rook_end = [|
      +0;  0;  0;  0;  0;  0;  0;  0;
      -5;  0;  0;  0;  0;  0;  0; -5;
      -5;  0;  0;  0;  0;  0;  0; -5;
      -5;  0;  0;  0;  0;  0;  0; -5;
      -5;  0;  0;  0;  0;  0;  0; -5;
      -5;  0;  0;  0;  0;  0;  0; -5;
      -5;  0;  0;  0;  0;  0;  0; -5;
      +0;  0;  0;  0;  0;  0;  0;  0;
    |]

    let queen_start = [|
      -20; -10; -10; -5; -5; -10; -10; -20;
      -10;  0;   0;   0;  0;  0;   0;   0;
      -10;  0;   5;   5;  5;  5;   0;  -10;
      -5;   0;   5;   5;  5;  5;   0;  -5;
      +0;   0;   5;   5;  5;  5;   0;  -5;
      -10;  5;   5;   5;  5;  5;   5;  -10;
      -10;  0;   5;   0;  0;  0;   0;  -10;
      -20; -10; -10; -5; -5; -10; -10; -20;
    |]

    let queen_end = [|
      -20; -10; -10; -5; -5; -10; -10; -20;
      -10;  0;   0;   0;  0;  0;   0;  -10;
      -10;  0;   5;   5;  5;  5;   0;  -10;
      -5;   0;   5;   5;  5;  5;   0;  -5;
      +0;   0;   5;   5;  5;  5;   0;  -5;
      -10;  5;   5;   5;  5;  5;   0;  -10;
      -10;  0;   5;   0;  0;  0;   0;  -10;
      -20; -10; -10; -5; -5; -10; -10; -20;
    |]

    let king_start = [|
      -30; -40; -40; -50; -50; -40; -40; -30;
      -30; -40; -40; -50; -50; -40; -40; -30;
      -30; -40; -40; -50; -50; -40; -40; -30;
      -30; -40; -40; -50; -50; -40; -40; -30;
      -20; -30; -30; -40; -40; -30; -30; -20;
      -10; -20; -20; -20; -20; -20; -20; -10;
      +20;  20;  0;   0;   0;   0;   20;  20;
      +20;  30;  10;  0;   0;   10;  20;  30;
    |]

    let king_end = [|
      -50; -40; -30; -20; -20; -30; -40; -50;
      -30; -20; -10;  0;   0;  -10; -20; -30;
      -30; -10;  20;  30;  30;  20; -10; -30;
      -30; -10;  30;  40;  40;  30; -10; -30;
      -30; -10;  30;  40;  40;  30; -10; -30;
      -30; -10;  20;  30;  30;  20; -10; -30;
      -30; -30;  0;   0;   0;   0;  -30; -30;
      -50; -30; -30; -30; -30; -30; -30; -50;
    |]

    let get t sq c =
      let sq = match c with
        | Piece.White -> Square.(with_rank_exn sq (7 - rank sq))
        | Piece.Black -> sq in
      Array.unsafe_get t @@ Square.to_int sq 
  end

  (* Weighted sum of piece placement (using piece-square tables). *)
  let go ?(swap = false) pos endgame =
    let c = if swap then Position.inactive pos else Position.active pos in
    Position.collect_color pos c |> List.fold ~init:0 ~f:(fun acc (sq, k) ->
        let open Tables in
        let start, end_ = match k with
          | Piece.Pawn   -> pawn_start,   pawn_end
          | Piece.Knight -> knight_start, knight_end
          | Piece.Bishop -> bishop_start, bishop_end
          | Piece.Rook   -> rook_start,   rook_end
          | Piece.Queen  -> queen_start,  queen_end
          | Piece.King   -> king_start,   king_end in
        let start = weigh_start (get start sq c) endgame in
        let end_  = weigh_end   (get end_  sq c) endgame in
        acc + start + end_)

  (* Relative placement advantage. *)
  let advantage pos our_endgame their_endgame =
    go pos their_endgame - go pos our_endgame ~swap:true
end

module Mop_up = struct
  (* Mop-up score (for endgames).

     See: https://www.chessprogramming.org/Mop-up_Evaluation
  *)
  let go ?(swap = false) pos endgame =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let b, b' = if swap then them, us else us, them in
    let our_king = Bb.(first_set_exn (b & Position.king pos)) in
    let their_king = Bb.(first_set_exn (b' & Position.king pos)) in
    let n =
      Square.manhattan_center their_king * 10 +
      (14 - Square.manhattan our_king their_king) * 4 in
    weigh_end n endgame

  let advantage pos
      our_endgame our_material
      their_endgame their_material =
    let us = if our_material > their_material + pawn_margin
      then go pos their_endgame else 0 in
    let them = if their_material > our_material + pawn_margin
      then go pos our_endgame ~swap:true else 0 in
    us - them
end

(* Overall evaluation. *)
let go pos =
  let our_pawns = Material.pawns pos in
  let their_pawns = Material.pawns pos ~swap:true in
  let our_material = Material.not_pawns pos in
  let their_material = Material.not_pawns pos ~swap:true in
  let our_endgame = Material.endgame_weight our_material in
  let their_endgame = Material.endgame_weight their_material in
  let material = (our_material + our_pawns) - (their_material + their_pawns) in
  let score =
    material +
    Mobility.advantage pos our_endgame their_endgame +
    Rook_open_file.advantage pos our_endgame their_endgame +
    Bishop_pair.advantage pos our_endgame their_endgame +
    King_pawn_shield.advantage pos our_endgame their_endgame +
    Passed_pawns.advantage pos our_endgame their_endgame +
    Doubled_pawns.advantage pos our_endgame their_endgame +
    Isolated_pawns.advantage pos our_endgame their_endgame +
    Placement.advantage pos our_endgame their_endgame +
    Mop_up.advantage pos
      our_endgame our_material
      their_endgame their_material in
  score
