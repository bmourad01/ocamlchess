open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let material_weight = 100

module Phase = struct
  type t = Opening | Endgame [@@deriving equal]

  let knight_phase = 1
  let bishop_phase = 1
  let rook_phase   = 2
  let queen_phase  = 4

  let total_phase  =
    knight_phase * 4 +
    bishop_phase * 4 +
    rook_phase   * 4 +
    queen_phase  * 2

  let max_phase = 256

  let of_position =
    let kinds = Piece.[
        Knight, knight_phase;
        Bishop, bishop_phase;
        Rook,   rook_phase;
        Queen,  queen_phase;
      ] in
    fun pos ->
      let phase =
        List.fold kinds ~init:total_phase ~f:(fun acc (k, w) ->
            acc - Bb.(count (Position.board_of_kind pos k)) * w) in
      ((phase * max_phase) + (total_phase / 2)) / total_phase

  let weigh_start phase score = (score * (max_phase - phase)) / max_phase
  let weigh_end phase score = (score * phase) / max_phase
end

module Material = struct
  (* In order: Pawn, Knight, Bishop, Rook, Queen *)
  let start_value = [|100; 300; 300; 500; 900|]
  let end_value   = [|140; 320; 330; 500; 900|]

  let go =
    let kinds = Piece.[Pawn; Knight; Bishop; Rook; Queen] in
    fun ?(swap = false) pos phase ->
      let us = Position.active_board pos in
      let them = Position.inactive_board pos in
      let b = if swap then them else us in
      List.fold kinds ~init:0 ~f:(fun acc k ->
          let b' = Position.board_of_kind pos k in
          let n = Bb.(count (b & b')) in
          let i = Piece.Kind.to_int k in
          match phase with
          | Phase.Opening -> acc + n * Array.unsafe_get start_value i
          | Phase.Endgame -> acc + n * Array.unsafe_get end_value   i)

  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

module Mobility = struct
  (* In order: pawn, knight, bishop, rook, queen, king. *)
  let start_bonus = [|0; 4; 3; 0; 0; 0|]
  let end_bonus   = [|1; 6; 2; 1; 1; 1|]
  let kinds       = Piece.[Knight; Bishop; Rook; Queen; King]

  (* Weighted sum of the "mobility" of the material. *)
  let go ?(swap = false) pos phase =
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
      match phase with
      | Phase.Opening -> n * start_bonus
      | Phase.Endgame -> n * end_bonus in
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
              match phase with
              | Phase.Opening -> acc + n * start_bonus
              | Phase.Endgame -> acc + n * end_bonus)) in
    mobility + pawn

  (* Relative mobility advantage. *)
  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

module Rook_open_file = struct
  let start_bonus = 20
  let end_bonus = 40

  (* Count the rooks on open files. This can also be measured by the mobility
     score, but we also give a bonus here. *)
  let go ?(swap = false) pos phase =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let occupied = Bb.(us + them) in
    let b = if swap then them else us in
    let rook = Bb.(b & Position.rook pos) in
    let score =
      List.init Square.File.count ~f:Bb.file_exn |>
      List.fold ~init:0 ~f:(fun acc f ->
          let b = Bb.(f & rook) in
          if Bb.(b <> empty && b = (f & occupied))
          then acc + 1 else acc) in
    match phase with
    | Phase.Opening -> score * start_bonus
    | Phase.Endgame -> score * end_bonus

  (* Relative advantage of rooks on open files. *)
  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

module Bishop_pair = struct
  let start_bonus = 45
  let end_bonus = 55

  let go ?(swap = false) pos phase =
    let us = Position.active_board pos in
    let them = Position.inactive_board pos in
    let b = if swap then them else us in
    let bishop = Bb.(b & Position.bishop pos) in
    let has_pair =
      Bb.(count (bishop & black)) <> 0 &&
      Bb.(count (bishop & white)) <> 0 in
    let score = Bool.to_int has_pair in
    match phase with 
    | Phase.Opening -> score * start_bonus
    | Phase.Endgame -> score * end_bonus

  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

module King_pawn_shield = struct
  let bonus = 10

  let idx i j = i + j * Piece.Color.count

  (* Given the king's square for a particular side, calculate the mask
     for pawns that can shield it (left, right, and center) from their
     starting ranks. *)
  let masks =
    let wf, wfl, wfr, wr = Int64.(lsl), Bb.file_a, Bb.file_h, Bb.rank_2 in
    let bf, bfl, bfr, br = Int64.(lsr), Bb.file_h, Bb.file_a, Bb.rank_7 in
    let tbl =
      let len = Piece.Color.count * Square.count in
      Array.create ~len Bb.empty in
    for i = 0 to Square.count - 1 do
      let sq = Int64.(1L lsl i) in
      let wm =
        Bb.(((of_int64 (wf sq 8)) +
             (of_int64 (wf sq 7) - wfr) +
             (of_int64 (wf sq 9) - wfl)) & wr) in
      let bm =
        Bb.(((of_int64 (bf sq 8)) +
             (of_int64 (bf sq 7) - bfr) +
             (of_int64 (bf sq 9) - bfl)) & br) in
      tbl.(idx Piece.Color.white i) <- wm;
      tbl.(idx Piece.Color.black i) <- bm;
    done;
    tbl

  (* In the opening phase, the king should be protected behind a group
     of pawns. *)
  let go ?(swap = false) pos phase = match phase with
    | Phase.Endgame -> 0
    | Phase.Opening ->
      let us = Position.active pos in
      let them = Position.inactive pos in
      let c = if swap then them else us in
      let b = Position.board_of_color pos c in
      let king = Bb.(b & Position.king pos) in
      let pawn = Bb.(b & Position.pawn pos) in
      let king_sq = Bb.first_set_exn king in
      let m = Array.unsafe_get masks @@
        idx (Piece.Color.to_int c) (Square.to_int king_sq) in
      let score = Bb.(count (m & pawn)) in
      score * bonus

  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

(* Pawn structure. *)
module Pawns = struct
  (* Give a bonus to having passed pawns, especially in endgame positions. *)
  module Passed = struct
    let start_bonus = 10
    let end_bonus = 70

    let idx i j = i + j * Piece.Color.count

    (* Given a pawn's square and color, get the squares left, right, and
       center for all the ranks that are ahead of it. *)
    let masks =
      let east b =
        let b' = Bb.to_int64 b in
        Bb.(of_int64 Int64.(b' lsl 1) - file_a) in
      let west b =
        let b' = Bb.to_int64 b in
        Bb.(of_int64 Int64.(b' lsr 1) - file_h) in
      let wf, wl, wr = Precalculated.Mask.north, east, west in
      let bf, bl, br = Precalculated.Mask.south, west, east in
      let tbl =
        let len = Piece.Color.count * Square.count in
        Array.create ~len Bb.empty in
      for i = 0 to Square.count - 1 do
        let sq = Square.of_int_exn i in
        let w = wf sq in
        let b = bf sq in
        tbl.(idx Piece.Color.white i) <- Bb.(w + wl w + wr w);
        tbl.(idx Piece.Color.black i) <- Bb.(w + bl b + br b);
      done;
      tbl

    let go pos phase c =
      let c' = Piece.Color.opposite c in
      let b = Position.board_of_color pos c in
      let b' = Position.board_of_color pos c' in
      let pawn = Bb.(b & Position.pawn pos) in
      let pawn' = Bb.(b' & Position.pawn pos) in
      let rec loop acc b =
        if Bb.(b = empty) then acc
        else
          let sq = Bb.first_set_exn b in
          (* If there's no enemy pawns that can intersect with this mask,
             then we have a passed pawn. *)
          let m = Array.unsafe_get masks @@
            idx (Piece.Color.to_int c) (Square.to_int sq) in
          let acc = acc + Bool.to_int Bb.((m & pawn') = empty) in
          loop acc @@ Bb.(b - (file_exn @@ Square.file sq)) in
      let score = loop 0 pawn in
      match phase with
      | Phase.Opening -> score * start_bonus
      | Phase.Endgame -> score * end_bonus

    let advantage pos phase = go pos phase White - go pos phase Black
  end

  (* Penalize having more than one pawn of the same color on the same file. *)
  module Doubled = struct
    let start_penalty = -20
    let end_penalty = -30

    let go pos phase c =
      let b = Position.board_of_color pos c in
      let pawn = Bb.(b & Position.pawn pos) in
      let score =
        List.init Square.File.count ~f:Bb.file_exn |>
        List.fold ~init:0 ~f:(fun acc f ->
            let n = Bb.(count (f & pawn)) in
            acc + max 0 (n - 1)) in
      match phase with
      | Phase.Opening -> score * start_penalty
      | Phase.Endgame -> score * end_penalty

    let advantage pos phase = go pos phase White - go pos phase Black
  end

  module Isolated = struct
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

    let go pos phase c =
      let b = Position.board_of_color pos c in
      let pawn = Bb.(b & Position.pawn pos) in
      let score =
        List.init Square.File.count ~f:ident |>
        List.fold ~init:0 ~f:(fun acc i ->
            (* Check if there are any pawns on the neighboring files
               that could potentially form a pawn chain. *)
            let f = Bb.file_exn i in
            let nf = Array.unsafe_get neighbor_files i in
            let isolated =
              Bb.(((f & pawn) <> empty) && ((nf & pawn) = empty)) in
            acc + Bool.to_int isolated) in
      match phase with
      | Phase.Opening -> score * start_penalty
      | Phase.Endgame -> score * end_penalty

    let advantage pos phase = go pos phase White - go pos phase Black
  end

  (* The pawn structure evaluations are static, therefore they aren't specific
     to any particular game or search history. Given this property, we can
     safely cache these results. *)
  let table = Hashtbl.create (module Int64)

  (* Evaluate the pawn structure. *)
  let go pos phase =
    let key = Position.pawn_hash pos in
    let start, end_ =
      match Hashtbl.find table key with
      | Some entry -> entry
      | None ->
        let start =
          Passed.advantage pos Opening +
          Doubled.advantage pos Opening +
          Isolated.advantage pos Opening in
        let end_ = 
          Passed.advantage pos Endgame +
          Doubled.advantage pos Endgame +
          Isolated.advantage pos Endgame in
        let data = start, end_ in
        Hashtbl.set table ~key ~data;
        data in
    (* To be able to cache these entries, we calculate them from white's
       perspective. If black is active, then it is as simple as negating
       the score. *)
    match phase, Position.active pos with
    | Phase.Opening, Piece.White -> start
    | Phase.Opening, Piece.Black -> -start
    | Phase.Endgame, Piece.White -> end_
    | Phase.Endgame, Piece.Black -> -end_
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
  let go ?(swap = false) pos phase =
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
        match phase with
        | Phase.Opening -> acc + get start sq c
        | Phase.Endgame -> acc + get end_  sq c)

  (* Relative placement advantage. *)
  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

module Mop_up = struct
  (* Mop-up score (for endgames).

     See: https://www.chessprogramming.org/Mop-up_Evaluation
  *)
  let go ?(swap = false) pos phase = match phase with
    | Phase.Opening -> 0
    | Phase.Endgame ->
      let us = Position.active_board pos in
      let them = Position.inactive_board pos in
      let b, b' = if swap then them, us else us, them in
      let our_king = Bb.(first_set_exn (b & Position.king pos)) in
      let their_king = Bb.(first_set_exn (b' & Position.king pos)) in
      Square.manhattan_center their_king * 10 +
      (14 - Square.manhattan our_king their_king) * 4

  let advantage pos phase = go pos phase - go pos phase ~swap:true
end

(* Overall evaluation. *)
let of_phase pos phase =
  Material.advantage pos phase +
  Mobility.advantage pos phase +
  Rook_open_file.advantage pos phase +
  Bishop_pair.advantage pos phase +
  King_pawn_shield.advantage pos phase +
  Pawns.go pos phase +
  Placement.advantage pos phase +
  Mop_up.advantage pos phase

let go pos =
  let phase = Phase.of_position pos in
  let start = Phase.weigh_start phase @@ of_phase pos Opening in
  let end_  = Phase.weigh_end   phase @@ of_phase pos Endgame in
  start + end_
