(* Evaluation of king placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

(* By number of defenders (pawn + minor) in our king's area. *)
let defenders = [|
  -37 $ -3;
  -17 $ 2;
  0 $ 6;
  11 $ 8;
  21 $ 8;
  32 $ 0;
  38 $ -14;
  10 $ -5;
  12 $ 6;
  12 $ 6;
  12 $ 6;
  12 $ 6;
|]

let finalize s =
  let mg = Score.mg s and eg = Score.eg s in
  (-mg * imax 0 mg / 720) $ (-(imax 0 eg) / 20)

let[@inline] go info c =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let knight = Position.knight info.pos in
  let bishop = Position.bishop info.pos in
  let queen = Position.queen info.pos in
  let their_queens = queen & them in
  (* Defenders. *)
  let eval =
    let b = (pawn + knight + bishop) & us in
    let d = count (b & our_info.king_area) in
    uget defenders d in
  (* Ignore when we have less than two attackers and no potential for
     a queen attacker. *)
  if Int.(our_info.num_king_attackers > 1 - count their_queens) then
    let na, na2 = ~~(our_info.attacks), ~~(our_info.attacks2) in
    (* Weak squares. *)
    let weak =
      let aq = Eval_info.attacked_by our_info Queen in
      let ak = Eval_info.attacked_by our_info King in
      their_info.attacks & na2 & (na + aq + ak) in
    (* Scale the number of attacks on the king by the size of the king
       area, which is typically 9 squares. *)
    let scaled_attacks =
      let n = 9.0 *. Float.of_int our_info.num_king_attacks in
      let d = Float.of_int @@ count our_info.king_area in
      Float.to_int (n /. d) in
    (* Squares that are safe for the enemy pieces. *)
    let safe = ~~them & (na + (weak & their_info.attacks2)) in
    (* Potential checking squares. *)
    let nthreats = Pre.knight our_info.king_square in
    let bthreats = Pre.bishop our_info.king_square all in
    let rthreats = Pre.rook   our_info.king_square all in
    let qthreats = bthreats + rthreats in
    let nchecks  = nthreats & safe & Eval_info.attacked_by their_info Knight in
    let bchecks  = bthreats & safe & Eval_info.attacked_by their_info Bishop in
    let rchecks  = rthreats & safe & Eval_info.attacked_by their_info Rook   in
    let qchecks  = qthreats & safe & Eval_info.attacked_by their_info Queen  in
    (* Eval_safety. *)
    let base = our_info.king_attackers +$ our_info.pawn_king_safety in
    let scale =
      (Eval_safety.attack_value      ** scaled_attacks)                     +$
      (Eval_safety.weak_squares      ** count (weak & our_info.king_area))  +$
      (Eval_safety.no_enemy_queens   ** Bool.to_int (their_queens = empty)) +$
      (Eval_safety.safe_queen_check  ** count qchecks)                      +$
      (Eval_safety.safe_rook_check   ** count rchecks)                      +$
      (Eval_safety.safe_bishop_check ** count bchecks)                      +$
      (Eval_safety.safe_knight_check ** count nchecks)                      in
    eval +$ finalize (base +$ scale +$ Eval_safety.adjustment)
  else eval
