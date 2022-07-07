open Core_kernel

module Bb = Bitboard
module Pre = Precalculated

let material_weight = 100

let sum2 (w, x) (y, z) = w + y, x + z
let sub2 (w, x) (y, z) = w - y, x - z
let scale2 (x, y) z = x * z, y * z

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
let[@specialise] evaluate (go : go) pos =
  sub2 (go pos White) (go pos Black)

module Material = struct
  let weights = Piece.[|
      Pawn,   (100, 140);
      Knight, (300, 320);
      Bishop, (300, 330);
      Rook,   (500, 500);
      Queen,  (900, 900);
    |]

  (* Count the material on the board. *)
  let evaluate = evaluate @@ fun pos c ->
    let b = Position.board_of_color pos c in
    Array.fold weights ~init:(0, 0) ~f:(fun acc (k, w)  ->
        sum2 acc @@ scale2 w Bb.(count (b & Position.board_of_kind pos k)))
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
          acc + Bool.to_int Bb.(b <> empty && b = (f & occupied))) in
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

module King_pawn_shield = struct
  let start_weight = 12
  let end_weight = 5

  let evaluate = evaluate @@ fun pos c ->
    let b = Position.board_of_color pos c in
    let king = Bb.(b & Position.king pos) in
    let pawn = Bb.(b & Position.pawn pos) in
    let king_sq = Bb.first_set_exn king in
    let score = Bb.(count (pawn & Pre.king king_sq)) in
    score * start_weight, score * end_weight
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

  module Chained = struct
    let start_weight = 5
    let end_weight = 11

    let go pos c =
      let occupied = Position.all_board pos in
      let us = Position.board_of_color pos c in
      let pawn = Bb.(us & Position.pawn pos) in
      let score = Bb.fold pawn ~init:0 ~f:(fun acc sq ->
          (* Count the number of pawns that are part of a chain. This
             is the immediate diagonal squares that are surrounding any
             particular pawn. *)
          let mask = Bb.(Pre.bishop sq occupied & Pre.king sq) in
          acc + Bb.(count (mask & pawn))) in
      score * start_weight, score * end_weight
  end

  (* Pawn structure usually doesn't change during the search, so it's
     possible to cache the previous evaluation results to get a
     performance improvement. *)
  let table = Zobrist.Table.create
      ~capacity:0x40000
      ~replace:(fun ~prev:_ _ _ -> true)
      ~age:(fun _ -> true)

  let go pos c = List.fold ~init:(0, 0) ~f:sum2 [
      Passed.go pos c;
      Doubled.go pos c;
      Isolated.go pos c;
      Chained.go pos c;
    ]

  (* Evaluate the overall pawn structure. *)
  let evaluate pos =
    let h = Position.pawn_hash pos in
    match Zobrist.Table.get_entry table h with
    | Some scores -> scores
    | None ->
      let scores = evaluate go pos in
      Zobrist.Table.set table h scores;
      scores
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

    let[@inline] get sq c t =
      Array.unsafe_get t @@
      Square.to_int @@ match (c : Piece.color) with
      | White -> flip_white sq
      | Black -> sq

    let[@inline] get sq c k =
      let i = Piece.Kind.to_int k in
      let module M = (val Array.unsafe_get kinds i) in
      get sq c M.start, get sq c M.end_
  end

  (* Weighted sum of piece placement (using piece-square tables). *)
  let evaluate = evaluate @@ fun pos c ->
    Position.collect_color pos c |>
    List.fold ~init:(0, 0) ~f:(fun acc (sq, k) ->
        sum2 acc @@ Tables.get sq c k)
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
  let evaluate pos material =
    let score =
      if material > margin
      then go pos White - go pos Black
      else 0 in
    0, score
end

let is_endgame pos = Phase.weight pos > 150

(* Overall evaluation of the position.

   This is a linear combination of weighted features, which serves as a
   heuristic for how advantageous a position is for the active player.
*)
let go pos =
  let phase_weight = Phase.weight pos in
  let material = Material.evaluate pos in
  let king_danger = ref 0, ref 0 in
  let mobility = Mobility.evaluate king_danger pos in
  let start, end_ = List.fold ~init:(0, 0) ~f:sum2 [
      material;
      mobility;
      Rook_open_file.evaluate pos;
      Bishop_pair.evaluate pos;
      King_danger.evaluate king_danger pos;
      King_pawn_shield.evaluate pos;
      Pawns.evaluate pos;
      Placement.evaluate pos;
      Mop_up.evaluate pos @@ snd material;
    ] in
  let start, end_ = match Position.active pos with
    | White -> start, end_
    | Black -> -start, -end_ in
  Phase.interpolate phase_weight start end_
