open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let[@inline] add2 (w, x) (y, z) = w + y, x + z
let[@inline] sub2 (w, x) (y, z) = w - y, x - z
let[@inline] scale2 (x, y) z = x * z, y * z
let[@inline] neg2 x = scale2 x (-1)

module Phase = struct
  let knight_phase = 1
  let bishop_phase = 1
  let rook_phase   = 2
  let queen_phase  = 4

  let total_phase  =
    knight_phase * 4 +
    bishop_phase * 4 +
    rook_phase   * 4 +
    queen_phase  * 2

  let maximum = 256

  let[@inline] weighted_count pos k p =
    p * (Bb.count @@ Position.board_of_kind pos k)
  
  (* Determine the current phase weight of the game. *)
  let[@inline] weight pos =
    let knight = weighted_count pos Knight knight_phase in
    let bishop = weighted_count pos Bishop bishop_phase in
    let rook   = weighted_count pos Rook   rook_phase   in
    let queen  = weighted_count pos Queen  queen_phase  in
    let weight = total_phase - knight - bishop - rook - queen in
    ((weight * maximum) + (total_phase / 2)) / total_phase

  (* Interpolate between opening and endgame scores. *)
  let[@inline] interpolate weight start end_ =
    ((start * (maximum - weight)) + (end_ * weight)) / maximum

  let[@inline] is_endgame pos = weight pos >= 160
end

(* Evaluates a particular feature of the position from a given player's
   perspective, and returns a pair of opening and endgame scores. *)
type go = Position.t -> Piece.color -> int * int

(* Calculate both opening and endgame advantage from white's perspective. *)
let[@specialise] evaluate (go : go) pos =
  sub2 (go pos White) (go pos Black)

let[@specialise] perspective c neg x = match c with
  | Piece.White -> x
  | Piece.Black -> neg x

module Material = struct
  let pawn_mg = 100
  let pawn_eg = 140
  let knight_mg = 300
  let knight_eg = 320
  let bishop_mg = 300
  let bishop_eg = 330
  let rook_mg = 500
  let rook_eg = 500
  let queen_mg = 900
  let queen_eg = 900

  let weights = Piece.[|
      Pawn,   (pawn_mg, pawn_eg);
      Knight, (knight_mg, knight_eg);
      Bishop, (bishop_mg, bishop_eg);
      Rook,   (rook_mg, rook_eg);
      Queen,  (queen_mg, queen_eg);
    |]

  (* Count the material on the board. *)
  let evaluate = evaluate @@ fun pos c ->
    let us = Position.board_of_color pos c in
    Array.fold weights ~init:(0, 0) ~f:(fun acc (k, w)  ->
        add2 acc @@ scale2 w Bb.(count (us & Position.board_of_kind pos k)))
end

module Mobility = struct
  let start_weight = 4
  let end_weight = 1

  (* Pair each kind with its bonus for controlling the center squares. We skip
     evaluating mobility for pawn and king. *)
  let kinds = Piece.[|
      Knight, 3;
      Bishop, 4;
      Rook,   1;
      Queen,  2;
    |]

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
    let score = Array.fold kinds ~init:0 ~f:(fun init (k, center_bonus) ->
        let f = match k with
          | Pawn   -> assert false
          | Knight -> Pre.knight
          | Bishop -> fun sq -> Pre.bishop sq occupied
          | Rook   -> fun sq -> Pre.rook   sq occupied
          | Queen  -> fun sq -> Pre.queen  sq occupied
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
  let start_weight = 5
  let end_weight = 10

  (* Count the rooks on open files. This can also be measured by the
     mobility score, but we also give a bonus here. *)
  let evaluate = evaluate @@ fun pos c ->
    let all = Position.all_board pos in
    let us = Position.board_of_color pos c in
    let rook = Bb.(us & Position.rook pos) in
    let score =
      List.init Square.File.count ~f:Bb.file_exn |>
      List.fold ~init:0 ~f:(fun acc f ->
          let b = Bb.(f & rook) in
          acc + Bool.to_int Bb.(b <> empty && b = (f & all))) in
    score * start_weight, score * end_weight
end

module Bishop_pair = struct
  let start_weight = 12
  let end_weight = 18

  (* Give a bonus if the player has a bishop pair. *)
  let evaluate = evaluate @@ fun pos c ->
    let us = Position.board_of_color pos c in
    let bishop = Bb.(us & Position.bishop pos) in
    let has_pair =
      Bb.((bishop & black) <> empty) &&
      Bb.((bishop & white) <> empty) in
    let score = Bool.to_int has_pair in
    score * start_weight, score * end_weight
end

module King_danger = struct
  let start_weight = -20
  let end_weight = -1

  (* Count the squares surrounding the king that are attacked. *)
  let evaluate (wk, bk) = evaluate @@ fun _ -> function
    | White -> !wk * start_weight, !wk * end_weight
    | Black -> !bk * start_weight, !bk * end_weight
end

module King_pawn_shield = struct
  let start_weight = 4
  let end_weight = 0

  let idx c sq = c + sq * Piece.Color.count

  let masks =
    let tbl =
      let len = Piece.Color.count * Square.count in
      Array.create ~len Bb.empty in
    for i = 0 to Square.count - 1 do
      let open Bb in
      let sq = !!(Square.of_int_exn i) in
      let w = (sq << 8) + ((sq << 7) - file_h) + ((sq << 9) - file_a) in
      let b = (sq >> 8) + ((sq >> 7) - file_a) + ((sq >> 9) - file_h) in
      tbl.(idx Piece.Color.white i) <- w & rank_2;
      tbl.(idx Piece.Color.black i) <- b & rank_7;
    done;
    tbl

  let evaluate = evaluate @@ fun pos c ->
    let b = Position.board_of_color pos c in
    let king = Bb.(b & Position.king pos) in
    let pawn = Bb.(b & Position.pawn pos) in
    let king_sq = Bb.first_set_exn king in
    let i = idx (Piece.Color.to_int c) (Square.to_int king_sq) in
    let score = Bb.(count (pawn & Array.unsafe_get masks i)) in
    score * start_weight, score * end_weight
end

(* Pawn structure. *)
module Pawns = struct
  module Passed = struct
    let start_weight = 2
    let end_weight = 5

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
      let all = Position.all_board pos in
      let us = Position.board_of_color pos c in
      let them = Bb.(all - us) in
      let pawn = Position.pawn pos in
      let our_pawn = Bb.(us & pawn) in
      let their_pawn = Bb.(them & pawn) in
      let score = Bb.fold our_pawn ~init:0 ~f:(fun acc sq ->
          let i = idx (Piece.Color.to_int c) (Square.to_int sq) in
          (* If there's no enemy pawns that can intersect with this mask,
             then we have a passed pawn. *)
          let m = Array.unsafe_get masks i in
          acc + Bool.to_int Bb.((m & their_pawn) = empty)) in
      score * start_weight, score * end_weight
  end

  module Doubled = struct
    let start_weight = -4
    let end_weight = -10

    let go pos c =
      let us = Position.board_of_color pos c in
      let pawn = Bb.(us & Position.pawn pos) in
      let score =
        List.init Square.File.count ~f:Bb.file_exn |>
        List.fold ~init:0 ~f:(fun acc f ->
            let n = Bb.(count (f & pawn)) in
            acc + max 0 (n - 1)) in
      score * start_weight, score * end_weight
  end

  module Isolated = struct
    let start_weight = -29
    let end_weight = -2

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

  (* Pawn structure usually doesn't change during the search, so it's
     possible to cache the previous evaluation results to get a
     performance improvement. *)
  let table = Hashtbl.create (module Int64)

  let go pos c = List.fold ~init:(0, 0) ~f:add2 [
      Passed.go pos c;
      Doubled.go pos c;
      Isolated.go pos c;
    ]

  (* Evaluate the overall pawn structure. *)
  let evaluate pos =
    Position.pawn_hash pos |> Hashtbl.find_or_add table
      ~default:(fun () -> evaluate go pos)
end

module Placement = struct
  (* Piece-square tables. *)
  module Tables = struct
    module Pawn = struct
      let start = [|
        0;  0;   0;   0;   0;   0;   0;  0;
        50; 50;  50;  50;  50;  50;  50; 50;
        10; 10;  20;  30;  30;  20;  10; 10;
        5;  5;   10;  25;  25;  10;  5;  5;
        0;  0;   0;   20;  20;  0;   0;  0;
        5; -5;  -10;  0;   0;  -10; -5;  5;
        5;  10;  10; -20; -20;  10;  10; 5;
        0;  0;   0;   0;   0;   0;   0;  0;
      |]

      let end_ = [|
        +0;   0;   0;   0;   0;   0;   0;   0;
        +80;  80;  80;  80;  80;  80;  80;  80;
        +60;  60;  60;  60;  60;  60;  60;  60;
        +40;  40;  40;  40;  40;  40;  40;  40;
        +20;  20;  20;  20;  20;  20;  20;  20;
        +0;   0;   0;   0;   0;   0;   0;   0;
        -20; -20; -20; -20; -20; -20; -20; -20;
        +0;   0;   0;   0;   0;   0;   0;   0;
      |]
    end

    module Knight = struct
      let start = [|
        -50; -40; -30; -30; -30; -30; -40; -50;
        -40; -20;  0;   0;   0;   0;  -20; -40;
        -30;  0;   10;  15;  15;  10;  0;  -30;
        -30;  5;   15;  20;  20;  15;  5;  -30;
        -30;  5;   10;  15;  15;  10;  0;  -30;
        -30;  5;   15;  20;  20;  15;  5;  -30;
        -40; -20;  0;   5;   5;   0;  -20; -40;
        -50; -40; -30; -30; -30; -30; -40; -50;
      |]

      let end_ = [|
        -50; -40; -30; -30; -30; -30; -40; -50;
        -40; -20;  0;   0;   0;   0;  -20; -40;
        -30;  0;   10;  15;  15;  10;  0;  -30;
        -30;  5;   15;  20;  20;  15;  5;  -30;
        -30;  0;   15;  20;  20;  15;  0;  -30;
        -30;  5;   10;  15;  15;  10;  5;  -30;
        -40; -20;  0;   5;   5;   0;  -20; -40;
        -50; -40; -30; -30; -30; -30; -40; -50;
      |]
    end

    module Bishop = struct
      let start = [|
        -20; -10; -10; -10; -10; -10; -10; -20;
        -10;  0;   0;   0;   0;   0;   0;  -10;
        -10;  0;   5;   10;  10;  5;   0;  -10;
        -10;  5;   5;   10;  10;  5;   5;  -10;
        -10;  0;   10;  10;  10;  10;  0;  -10;
        -10;  10;  10;  10;  10;  10;  10; -10;
        -10;  5;   0;   0;   0;   0;   5;  -10;
        -20; -10; -10; -10; -10; -10; -10; -20;
      |]

      let end_ = [|
        -20; -10; -10; -10; -10; -10; -10; -20;
        -10;  0;   0;   0;   0;   0;   0;  -10;
        -10;  0;   5;   10;  10;  5;   0;  -10;
        -10;  5;   5;   10;  10;  5;   5;  -10;
        -10;  0;   10;  10;  10;  10;  0;  -10;
        -10;  10;  10;  10;  10;  10;  10; -10;
        -10;  5;   0;   0;   0;   0;   5;  -10;
        -20; -10; -10; -10; -10; -10; -10; -20;
      |]
    end

    module Rook = struct
      let start = [|
        +0; 0;  0;  0;  0;  0;  0;  0;
        +5; 10; 10; 10; 10; 10; 10; 5;
        -5; 0;  0;  0;  0;  0;  0; -5;
        -5; 0;  0;  0;  0;  0;  0; -5;
        -5; 0;  0;  0;  0;  0;  0; -5;
        -5; 0;  0;  0;  0;  0;  0; -5;
        -5; 0;  0;  0;  0;  0;  0; -5;
        +0; 0;  0;  0;  5;  5;  0;  0;
      |]

      let end_ = [|
        +0;  0;  0;  0;  0;  0;  0;  0;
        -5;  0;  0;  0;  0;  0;  0; -5;
        -5;  0;  0;  0;  0;  0;  0; -5;
        -5;  0;  0;  0;  0;  0;  0; -5;
        -5;  0;  0;  0;  0;  0;  0; -5;
        -5;  0;  0;  0;  0;  0;  0; -5;
        -5;  0;  0;  0;  0;  0;  0; -5;
        +0;  0;  0;  0;  0;  0;  0;  0;
      |]
    end

    module Queen = struct
      let start = [|
        -20; -10; -10; -5; -5; -10; -10; -20;
        -10;  0;   0;   0;  0;  0;   0;   0;
        -10;  0;   5;   5;  5;  5;   0;  -10;
        -5;   0;   5;   5;  5;  5;   0;  -5;
        +0;   0;   5;   5;  5;  5;   0;  -5;
        -10;  5;   5;   5;  5;  5;   5;  -10;
        -10;  0;   5;   0;  0;  0;   0;  -10;
        -20; -10; -10; -5; -5; -10; -10; -20;
      |]

      let end_ = [|
        -20; -10; -10; -5; -5; -10; -10; -20;
        -10;  0;   0;   0;  0;  0;   0;  -10;
        -10;  0;   5;   5;  5;  5;   0;  -10;
        -5;   0;   5;   5;  5;  5;   0;  -5;
        +0;   0;   5;   5;  5;  5;   0;  -5;
        -10;  5;   5;   5;  5;  5;   0;  -10;
        -10;  0;   5;   0;  0;  0;   0;  -10;
        -20; -10; -10; -5; -5; -10; -10; -20;
      |]
    end

    module King = struct
      let start = [|
        -30; -40; -40; -50; -50; -40; -40; -30;
        -30; -40; -40; -50; -50; -40; -40; -30;
        -30; -40; -40; -50; -50; -40; -40; -30;
        -30; -40; -40; -50; -50; -40; -40; -30;
        -20; -30; -30; -40; -40; -30; -30; -20;
        -10; -20; -20; -20; -20; -20; -20; -10;
        +20;  20;  0;   0;   0;   0;   20;  20;
        +20;  30;  10;  0;   0;   10;  20;  30;
      |]

      let end_ = [|
        -50; -40; -30; -20; -20; -30; -40; -50;
        -30; -20; -10;  0;   0;  -10; -20; -30;
        -30; -10;  20;  30;  30;  20; -10; -30;
        -30; -10;  30;  40;  40;  30; -10; -30;
        -30; -10;  30;  40;  40;  30; -10; -30;
        -30; -10;  20;  30;  30;  20; -10; -30;
        -30; -30;  0;   0;   0;   0;  -30; -30;
        -50; -30; -30; -30; -30; -30; -30; -50;
      |]
    end

    module type S = sig
      val start : int array
      val end_  : int array
    end

    let kinds : (module S) array = [|
      (module Pawn);
      (module Knight);
      (module Bishop);
      (module Rook);
      (module Queen);
      (module King);
    |]

    let[@inline] flip_white sq =
      Square.(with_rank_exn sq (Rank.eight - rank sq))

    let[@inline] square sq = function
      | Piece.White -> flip_white sq
      | Piece.Black -> sq

    let[@inline] of_table sq c t =
      Array.unsafe_get t @@ Square.to_int @@ square sq c

    let[@inline] of_kind sq c k =
      let i = Piece.Kind.to_int k in
      let module M = (val Array.unsafe_get kinds i) in
      of_table sq c M.start, of_table sq c M.end_
  end

  (* Weighted sum of piece placement (using piece-square tables). *)
  let evaluate = evaluate @@ fun pos c ->
    Position.collect_color pos c |>
    List.fold ~init:(0, 0) ~f:(fun acc (sq, k) ->
        add2 acc @@ Tables.of_kind sq c k)
end

module Mop_up = struct
  let mc_weight = 10
  let md_weight = 4

  (* Margin for material advantage. *)
  let margin = Material.(snd @@ snd @@ weights.(Piece.Kind.pawn)) * 2

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
  let evaluate material = evaluate @@ fun pos c ->
    let material = perspective c Int.neg material in
    0, if material >= margin then go pos c else 0
end

(* Overall evaluation of the position.

   This is a linear combination of weighted features, which serves as a
   heuristic for how advantageous a position is for the active player.
*)
let go pos =
  let phase_weight = Phase.weight pos in
  let material = Material.evaluate pos in
  let king_danger = ref 0, ref 0 in
  let mobility = Mobility.evaluate king_danger pos in
  let start, end_ = List.fold ~init:(0, 0) ~f:add2 [
      material;
      mobility;
      Rook_open_file.evaluate pos;
      Bishop_pair.evaluate pos;
      King_danger.evaluate king_danger pos;
      King_pawn_shield.evaluate pos;
      Pawns.evaluate pos;
      Placement.evaluate pos;
      Mop_up.evaluate (snd material) pos;
    ] |> perspective (Position.active pos) neg2 in
  Phase.interpolate phase_weight start end_
