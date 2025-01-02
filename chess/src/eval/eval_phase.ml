(* Game phase (i.e. middle or endgame). *)

let knight_phase = 1
let bishop_phase = 1
let rook_phase   = 2
let queen_phase  = 4

let total_phase  =
  knight_phase * 4 +
  bishop_phase * 4 +
  rook_phase   * 4 +
  queen_phase  * 2

let[@inline] weighted_count pos k p =
  p * Bitboard.count (Position.board_of_kind pos k)

(* Determine the current phase weight of the game based on the
   available material. *)
let[@inline] weight pos =
  weighted_count pos Knight knight_phase +
  weighted_count pos Bishop bishop_phase +
  weighted_count pos Rook   rook_phase   +
  weighted_count pos Queen  queen_phase

(* Interpolate between middle and endgame scores. *)
let[@inline] interpolate pos eval =
  let scale = Eval_scale.go pos eval in
  let w = weight pos in
  let mg = Eval_score.mg eval * w in
  let eg = Eval_score.eg eval * (total_phase - w) * scale / Eval_scale.normal in
  (mg + eg) / total_phase
