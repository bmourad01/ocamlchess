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

  (* Determine the current phase weight of the game. *)
  let weight pos =
    let f k p  = Bb.(count (Position.board_of_kind pos k)) * p in
    let knight = f Knight knight_phase in
    let bishop = f Bishop bishop_phase in
    let rook   = f Rook   rook_phase   in
    let queen  = f Queen  queen_phase  in
    let weight = total_phase - knight - bishop - rook - queen in
    ((weight * max_phase) + (total_phase / 2)) / total_phase

  (* Interpolate between opening and endgame scores. *)
  let interpolate weight start end_ =
    ((start * (max_phase - weight)) + (end_ * weight)) / max_phase
end

type go = Position.t -> Phase.t -> Piece.color -> int

let advantage (go : go) pos phase =
  let score = go pos phase White - go pos phase Black in
  match Position.active pos with
  | White -> score
  | Black -> -score

module Material = struct
  (* In order: Pawn, Knight, Bishop, Rook, Queen *)
  let start_weight = [|100; 300; 300; 500; 900|]
  let end_weight   = [|140; 320; 330; 500; 900|]
  let kinds        = Piece.[|Pawn; Knight; Bishop; Rook; Queen|]

  let advantage = advantage @@ fun pos phase c ->
    let b = Position.board_of_color pos c in
    Array.fold kinds ~init:0 ~f:(fun acc k ->
        let n = Bb.(count (b & Position.board_of_kind pos k)) in
        let i = Piece.Kind.to_int k in
        match phase with
        | Phase.Opening -> acc + n * Array.unsafe_get start_weight i
        | Phase.Endgame -> acc + n * Array.unsafe_get end_weight   i)
end

module Mobility = struct
  (* In order: pawn, knight, bishop, rook, queen, king. *)
  let start_weight = [|0; 4; 3; 0; 0; 0|]
  let end_weight   = [|1; 6; 2; 1; 1; 1|]
  let kinds       = Piece.[|Knight; Bishop; Rook; Queen; King|]

  (* Weighted sum of the "mobility" of the material. *)
  let advantage = advantage @@ fun pos phase c ->
    let occupied = Position.all_board pos in
    let us = Position.board_of_color pos c in
    let them = Bb.(occupied - us) in
    (* For pawns, we not only want to evaluate attacked squares but
       also the ability to push to new ranks. *)
    let pawn =
      let pawn = Bb.(us & Position.pawn pos) in
      let single, double, attack = match c with
        | Piece.White ->
          let single = Bb.((pawn << 8) - occupied) in
          let double = Bb.(((single & rank_3) << 8) - occupied) in
          let attack = Bb.(((pawn << 9) - file_a) + ((pawn << 7) - file_h)) in
          single, double, attack
        | Piece.Black ->
          let single = Bb.((pawn >> 8) - occupied) in
          let double = Bb.(((single & rank_6) >> 8) - occupied) in
          let attack = Bb.(((pawn >> 9) - file_h) + ((pawn >> 7) - file_a)) in
          single, double, attack in
      let attackable = Bb.(them - Position.king pos) in
      let n = Bb.(count (single + double + (attack & attackable))) in
      let i = Piece.Kind.pawn in
      match phase with
      | Phase.Opening -> n * Array.unsafe_get start_weight i
      | Phase.Endgame -> n * Array.unsafe_get end_weight   i in
    (* For the rest, count the number of attacked squares. *)
    let rest = Array.fold kinds ~init:0 ~f:(fun acc k ->
        let f = match k with
          | Pawn   -> assert false
          | Knight -> Pre.knight
          | Bishop -> fun sq -> Pre.bishop sq occupied
          | Rook   -> fun sq -> Pre.rook   sq occupied
          | Queen  -> fun sq -> Pre.queen  sq occupied
          | King   -> Pre.king in
        let i = Piece.Kind.to_int k in
        let weight = match phase with
          | Phase.Opening -> Array.unsafe_get start_weight i
          | Phase.Endgame -> Array.unsafe_get end_weight   i in
        Bb.(Position.board_of_kind pos k & us) |>
        Bb.fold ~init:acc ~f:(fun acc sq ->
            acc + Bb.(count (f sq - us)) * weight)) in
    pawn + rest
end

module Rook_open_file = struct
  let start_weight = 20
  let end_weight = 40

  (* Count the rooks on open files. This can also be measured by the
     mobility score, but we also give a bonus here. *)
  let advantage = advantage @@ fun pos phase c ->
    let occupied = Position.all_board pos in
    let b = Position.board_of_color pos c in
    let rook = Bb.(b & Position.rook pos) in
    let score =
      List.init Square.File.count ~f:Bb.file_exn |>
      List.fold ~init:0 ~f:(fun acc f ->
          let b = Bb.(f & rook) in
          if Bb.(b <> empty && b = (f & occupied))
          then acc + 1 else acc) in
    match phase with
    | Phase.Opening -> score * start_weight
    | Phase.Endgame -> score * end_weight
end

module Bishop_pair = struct
  let start_weight = 45
  let end_weight = 55

  (* Give a bonus if the player has a bishop pair. *)
  let advantage = advantage @@ fun pos phase c ->
    let b = Position.board_of_color pos c in
    let bishop = Bb.(b & Position.bishop pos) in
    let has_pair =
      Bb.(count (bishop & black)) <> 0 &&
      Bb.(count (bishop & white)) <> 0 in
    let score = Bool.to_int has_pair in
    match phase with 
    | Phase.Opening -> score * start_weight
    | Phase.Endgame -> score * end_weight
end

module King_pawn_shield = struct
  let weight = 10

  let idx c sq = c + sq * Piece.Color.count

  (* Given the king's square for a particular side, calculate the mask
     for pawns that can shield it (left, right, and center) from their
     starting ranks. *)
  let masks =
    let tbl =
      let len = Piece.Color.count * Square.count in
      Array.create ~len Bb.empty in
    for i = 0 to Square.count - 1 do
      let open Bb in
      let sq = !!(Square.of_int_unsafe i) in
      let a, h = file_a, file_h in
      let wm = ((sq << 8) + ((sq << 7) - h) + ((sq << 9) - a)) & rank_2 in
      let bm = ((sq >> 8) + ((sq >> 7) - a) + ((sq >> 9) - h)) & rank_7 in
      tbl.(idx Piece.Color.white i) <- wm;
      tbl.(idx Piece.Color.black i) <- bm;
    done;
    tbl

  (* In the opening phase, the king should be protected behind a group
     of pawns. *)
  let advantage = advantage @@ fun pos phase c -> match phase with
    | Phase.Endgame -> 0
    | Phase.Opening ->
      let b = Position.board_of_color pos c in
      let king = Bb.(b & Position.king pos) in
      let pawn = Bb.(b & Position.pawn pos) in
      let king_sq = Bb.first_set_exn king in
      let m = Array.unsafe_get masks @@
        idx (Piece.Color.to_int c) (Square.to_int king_sq) in
      let score = Bb.(count (m & pawn)) in
      score * weight
end

module King_danger = struct
  let start_weight = -14
  let end_weight = 3

  (* Count the squares surrounding the king that are attacked. *)
  let advantage = advantage @@ fun pos phase c ->
    let box = Pre.king @@ Bb.first_set_exn @@ Position.king pos in
    let attacks = Position.Attacks.all pos @@ Piece.Color.opposite c in
    let score = Bb.(count (box & attacks)) in
    match phase with
    | Phase.Opening -> score * start_weight
    | Phase.Endgame -> score * end_weight
end

(* Pawn structure. *)
module Pawns = struct
  (* Give a bonus to having passed pawns, especially in endgame positions. *)
  module Passed = struct
    let start_weight = 10
    let end_weight = 70

    let idx c sq = c + sq * Piece.Color.count

    (* Given a pawn's square and color, get the squares left, right, and
       center for all the ranks that are ahead of it. *)
    let masks =
      let tbl =
        let len = Piece.Color.count * Square.count in
        Array.create ~len Bb.empty in
      for i = 0 to Square.count - 1 do
        let open Bb in
        let sq = Square.of_int_exn i in
        let n = Pre.Mask.north sq in
        let s = Pre.Mask.south sq in
        let a, h = file_a, file_h in
        let w = n + ((n << 1) - a) + ((n >> 1) - h) in
        let b = s + ((s >> 1) - h) + ((s << 1) - a) in
        tbl.(idx Piece.Color.white i) <- w;
        tbl.(idx Piece.Color.black i) <- b;
      done;
      tbl

    let go pos phase c =
      let us = Position.board_of_color pos c in
      let them = Position.board_of_color pos @@ Piece.Color.opposite c in
      let our_pawn = Bb.(us & Position.pawn pos) in
      let their_pawn = Bb.(them & Position.pawn pos) in
      let rec loop acc b =
        if Bb.(b = empty) then acc
        else
          let sq = Bb.first_set_exn b in
          (* If there's no enemy pawns that can intersect with this mask,
             then we have a passed pawn. *)
          let m = Array.unsafe_get masks @@
            idx (Piece.Color.to_int c) (Square.to_int sq) in
          let acc = acc + Bool.to_int Bb.((m & their_pawn) = empty) in
          loop acc @@ Bb.(b - (file_exn @@ Square.file sq)) in
      let score = loop 0 our_pawn in
      match phase with
      | Phase.Opening -> score * start_weight
      | Phase.Endgame -> score * end_weight
  end

  (* Penalize having more than one pawn of the same color on the same file. *)
  module Doubled = struct
    let start_weight = -20
    let end_weight = -30

    let go pos phase c =
      let b = Position.board_of_color pos c in
      let pawn = Bb.(b & Position.pawn pos) in
      let score =
        List.init Square.File.count ~f:Bb.file_exn |>
        List.fold ~init:0 ~f:(fun acc f ->
            let n = Bb.(count (f & pawn)) in
            acc + max 0 (n - 1)) in
      match phase with
      | Phase.Opening -> score * start_weight
      | Phase.Endgame -> score * end_weight
  end

  module Isolated = struct
    let start_weight = -15
    let end_weight = -30

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
      | Phase.Opening -> score * start_weight
      | Phase.Endgame -> score * end_weight
  end

  (* Evaluate the overall pawn structure. *)
  let advantage = advantage @@ fun pos phase c ->
    Passed.go pos phase c +
    Doubled.go pos phase c +
    Isolated.go pos phase c
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
      -10;  0;   0;   0;   0;   0;   0;  -10;
      -10;  0;   5;   10;  10;  5;   0;  -10;
      -10;  5;   5;   10;  10;  5;   5;  -10;
      -10;  0;   10;  10;  10;  10;  0;  -10;
      -10;  10;  10;  10;  10;  10;  10; -10;
      -10;  5;   0;   0;   0;   0;   5;  -10;
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
  let advantage = advantage @@ fun pos phase c ->
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
end

module Mop_up = struct
  let manhattan_center_weight = 10
  let manhattan_distance_weight = 4

  (* Margin for material advantage. *)
  let margin = Material.end_weight.(Piece.Kind.pawn) * 2

  (* Mop-up: https://www.chessprogramming.org/Mop-up_Evaluation *)
  let go pos c =
    let us = Position.board_of_color pos c in
    let them = Position.board_of_color pos @@ Piece.Color.opposite c in
    let king = Position.king pos in
    let our_king = Bb.(first_set_exn (us & king)) in
    let their_king = Bb.(first_set_exn (them & king)) in
    let mc = Square.manhattan_center their_king in
    let md = Square.manhattan our_king their_king  in
    (mc * manhattan_center_weight) + (14 - md) * manhattan_distance_weight

  (* 1. This only matters in endgame positions.

     2. We don't want to bother evaluating this feature without a clear
        material advantage.
  *)
  let advantage pos phase material =
    if Phase.equal phase Endgame && material > margin then
      let score = go pos White - go pos Black in
      match Position.active pos with
      | White -> score
      | Black -> -score
    else 0
end

let is_endgame pos = Phase.weight pos > 150

(* Phase-specific evaluation. *)
let of_phase pos phase =
  let material = Material.advantage pos phase in
  material +
  Mobility.advantage pos phase +
  Rook_open_file.advantage pos phase +
  Bishop_pair.advantage pos phase +
  King_pawn_shield.advantage pos phase +
  King_danger.advantage pos phase +
  Pawns.advantage pos phase +
  Placement.advantage pos phase +
  Mop_up.advantage pos phase material

(* Overall evaluation. *)
let go pos =
  let phase = Phase.weight pos in
  let start = of_phase pos Opening in
  let end_  = of_phase pos Endgame in
  Phase.interpolate phase start end_
