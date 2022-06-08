open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let material_weight = 100

module Phase = struct
  type t = Opening | Endgame

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
  let[@inline] weight pos =
    let f k p  = Bb.(count (Position.board_of_kind pos k)) * p in
    let knight = f Knight knight_phase in
    let bishop = f Bishop bishop_phase in
    let rook   = f Rook   rook_phase   in
    let queen  = f Queen  queen_phase  in
    let weight = total_phase - knight - bishop - rook - queen in
    ((weight * max_phase) + (total_phase / 2)) / total_phase

  (* Interpolate between opening and endgame scores. *)
  let[@inline] interpolate weight start end_ =
    ((start * (max_phase - weight)) + (end_ * weight)) / max_phase
end

(* Evaluates a particular feature of the position from a given player's
   perspective, and returns a pair of opening and endgame scores. *)
type go = Position.t -> Piece.color -> int * int

(* Calculate both opening and endgame advantage from white's perspective. *)
let evaluate (go : go) pos =
  let start_white, end_white = go pos White in
  let start_black, end_black = go pos Black in
  start_white - start_black, end_white - end_black

module Material = struct
  (* In order: Pawn, Knight, Bishop, Rook, Queen *)
  let start_weight = [|100; 300; 300; 500; 900|]
  let end_weight   = [|140; 320; 330; 500; 900|]
  let kinds        = Piece.[|Pawn; Knight; Bishop; Rook; Queen|]

  let evaluate = evaluate @@ fun pos c ->
    let b = Position.board_of_color pos c in
    Array.fold kinds ~init:(0, 0) ~f:(fun (s, e) k ->
        let n = Bb.(count (b & Position.board_of_kind pos k)) in
        let i = Piece.Kind.to_int k in
        let start = n * Array.unsafe_get start_weight i in
        let end_ = n * Array.unsafe_get end_weight i in
        s + start, e + end_)
end

module Mobility = struct
  let start_weight = 5
  let end_weight = 1
  let center_bonus = 2

  (* Calculating pawn and king mobility is too slow and not especially
     important. These pieces are better evaluated by other features. *)
  let kinds = Piece.[|Knight; Bishop; Rook; Queen|]

  (* Weighted sum of the "mobility" of the material. Also, collect the number
     of attacks near the squares immediately surrounding the enemy king. This
     can be used later to evaluate king safety. *)
  let evaluate (wk, bk) = evaluate @@ fun pos c ->
    let occupied = Position.all_board pos in
    let us = Position.board_of_color pos c in
    let them = Bb.(occupied - us) in
    let king_sq = Bb.(first_set_exn (them & Position.king pos)) in
    let box = Pre.king king_sq in
    let king_danger = match c with White -> bk | Black -> wk in
    let bishop = Bb.(Position.bishop pos & us) in
    let rook = Bb.(Position.rook pos & us) in
    let queen = Bb.(Position.queen pos & us) in
    let bq = Bb.(bishop + queen) in
    let rq = Bb.(rook + queen) in
    let brq = Bb.(bishop + rook + queen) in
    let score = Array.fold kinds ~init:0 ~f:(fun init k ->
        let f = match k with
          | Pawn   -> assert false
          | Knight -> Pre.knight
          | Bishop -> fun sq -> Pre.bishop sq Bb.(occupied - bq)
          | Rook   -> fun sq -> Pre.rook   sq Bb.(occupied - rq)
          | Queen  -> fun sq -> Pre.queen  sq Bb.(occupied - brq)
          | King   -> assert false in
        Bb.(Position.board_of_kind pos k & us) |>
        Bb.fold ~init ~f:(fun score sq ->
            let b = Bb.(f sq - us) in
            king_danger := !king_danger + Bb.(count (b & box));
            Bb.(count (b & center)) * center_bonus +
            Bb.(count (b - center)))) in
    score * start_weight, score * end_weight
end

module Rook_open_file = struct
  let start_weight = 20
  let end_weight = 40

  (* Count the rooks on open files. This can also be measured by the
     mobility score, but we also give a bonus here. *)
  let evaluate = evaluate @@ fun pos c ->
    let occupied = Position.all_board pos in
    let b = Position.board_of_color pos c in
    let rook = Bb.(b & Position.rook pos) in
    let score =
      List.init Square.File.count ~f:Bb.file_exn |>
      List.fold ~init:0 ~f:(fun acc f ->
          let b = Bb.(f & rook) in
          if Bb.(b <> empty && b = (f & occupied))
          then acc + 1 else acc) in
    score * start_weight, score * end_weight
end

module Bishop_pair = struct
  let start_weight = 45
  let end_weight = 55

  (* Give a bonus if the player has a bishop pair. *)
  let evaluate = evaluate @@ fun pos c ->
    let b = Position.board_of_color pos c in
    let bishop = Bb.(b & Position.bishop pos) in
    let has_pair =
      Bb.((bishop & black) <> empty) &&
      Bb.((bishop & white) <> empty) in
    let score = Bool.to_int has_pair in
    score * start_weight, score * end_weight
end

module King_danger = struct
  let start_weight = -14
  let end_weight = 3

  (* Count the squares surrounding the king that are attacked. *)
  let evaluate (wk, bk) = evaluate @@ fun _ -> function
    | White -> !wk * start_weight, !wk * end_weight
    | Black -> !bk * start_weight, !bk * end_weight
end

(* Pawn structure. *)
module Pawns = struct
  module Passed = struct
    let start_weight = 2
    let end_weight = 54

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

    let go pos c =
      let us = Position.board_of_color pos c in
      let them = Position.board_of_color pos @@ Piece.Color.opposite c in
      let our_pawn = Bb.(us & Position.pawn pos) in
      let their_pawn = Bb.(them & Position.pawn pos) in
      let score = Bb.fold our_pawn ~init:0 ~f:(fun acc sq ->
          let i = idx (Piece.Color.to_int c) (Square.to_int sq) in
          (* If there's no enemy pawns that can intersect with this mask,
             then we have a passed pawn. *)
          let m = Array.unsafe_get masks i in
          acc + Bool.to_int Bb.((m & their_pawn) = empty)) in
      score * start_weight, score * end_weight
  end

  module Doubled = struct
    let start_weight = 4
    let end_weight = -10

    let go pos c =
      let b = Position.board_of_color pos c in
      let pawn = Bb.(b & Position.pawn pos) in
      let score =
        List.init Square.File.count ~f:Bb.file_exn |>
        List.fold ~init:0 ~f:(fun acc f ->
            let n = Bb.(count (f & pawn)) in
            acc + max 0 (n - 1)) in
      score * start_weight, score * end_weight
  end

  module Isolated = struct
    let start_weight = -29
    let end_weight = 0

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

    let go pos c =
      let us = Position.board_of_color pos c in
      let pawn = Bb.(us & Position.pawn pos) in
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
      score * start_weight, score * end_weight
  end

  (* Pawns shielding the king. *)
  module Shield = struct
    let start_weight = 12
    let end_weight = 5

    let go pos c =
      let b = Position.board_of_color pos c in
      let king = Bb.(b & Position.king pos) in
      let pawn = Bb.(b & Position.pawn pos) in
      let king_sq = Bb.first_set_exn king in
      let score = Bb.(count (pawn & Pre.king king_sq)) in
      score * start_weight, score * end_weight
  end

  module Chained = struct
    let start_weight = 5
    let end_weight = 11

    let go pos c =
      let occupied = Position.all_board pos in
      let us = Position.board_of_color pos c in
      let score =
        Bb.(us & Position.pawn pos) |> Bb.fold ~init:0 ~f:(fun acc sq ->
            let mask = Bb.(Pre.bishop sq occupied & Pre.king sq) in
            acc + Bb.(count (mask & us))) in
      score * start_weight, score * end_weight
  end

  (* Evaluate the overall pawn structure. *)
  let evaluate = evaluate @@ fun pos c ->
    let passed_start, passed_end = Passed.go pos c in
    let doubled_start, doubled_end = Doubled.go pos c in
    let isolated_start, isolated_end = Isolated.go pos c in
    let chained_start, chained_end = Chained.go pos c in
    let shield_start, shield_end = Shield.go pos c in
    let start =
      passed_start +
      doubled_start +
      isolated_start +
      chained_start +
      shield_start in
    let end_ =
      passed_end +
      doubled_end +
      isolated_end +
      chained_end +
      shield_end in
    start, end_
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

    let kinds = [|
      pawn_start,   pawn_end;
      knight_start, knight_end;
      bishop_start, bishop_end;
      rook_start,   rook_end;
      queen_start,  queen_end;
      king_start,   king_end;
    |]
  end

  (* Weighted sum of piece placement (using piece-square tables). *)
  let evaluate = evaluate @@ fun pos c ->
    Position.collect_color pos c |>
    List.fold ~init:(0, 0) ~f:(fun (s, e) (sq, k) ->
        let open Tables in
        let start, end_ = Array.unsafe_get kinds @@ Piece.Kind.to_int k in
        s + get start sq c, e + get end_ sq c)
end

module Mop_up = struct
  let mc_weight = 10
  let md_weight = 4

  (* Margin for material advantage. *)
  let margin = Material.end_weight.(Piece.Kind.pawn) * 2

  (* Mop-up: https://www.chessprogramming.org/Mop-up_Evaluation

     In endgame positions, we may want to force the enemy king away from
     the center of the board. Among other means, this can be accomplished
     by minimizing the distance between both kings.
  *)
  let go pos c =
    let us = Position.board_of_color pos c in
    let them = Position.board_of_color pos @@ Piece.Color.opposite c in
    let king = Position.king pos in
    let our_king = Bb.(first_set_exn (us & king)) in
    let their_king = Bb.(first_set_exn (them & king)) in
    let mc = Square.manhattan_center their_king in
    let md = Square.manhattan our_king their_king  in
    (mc * mc_weight) + (14 - md) * md_weight

  (* 1. This only matters in endgame positions.

     2. We don't want to bother evaluating this feature without a clear
        material advantage.
  *)
  let evaluate pos material =
    let score =
      if material > margin
      then go pos White - go pos Black
      else 0 in
    0, score
end

let is_endgame pos = Phase.weight pos > 150

let sum2 (w, x) (y, z) = w + y, x + z

(* Overall evaluation of the position.

   This is a linear combination of weighted features, which serves as a
   heuristic for how advantageous a position is for the active player.
*)
let go pos =
  let phase_weight = Phase.weight pos in
  let material = Material.evaluate pos in
  let king_danger = ref 0, ref 0 in
  let mobility = Mobility.evaluate king_danger pos in
  let start, end_ = Array.fold ~init:(0, 0) ~f:sum2 [|
      material;
      mobility;
      Rook_open_file.evaluate pos;
      Bishop_pair.evaluate pos;
      King_danger.evaluate king_danger pos;
      Pawns.evaluate pos;
      Placement.evaluate pos;
      Mop_up.evaluate pos @@ snd material;
    |] in
  let start, end_ = match Position.active pos with
    | White -> start, end_
    | Black -> -start, -end_ in
  Phase.interpolate phase_weight start end_
