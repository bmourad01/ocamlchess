(* Evaluation of knight placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

module Outpost = struct
  let t = [|
    12 $ -32;
    40 $ 0;
    7 $ -24;
    21 $ -3;
  |]

  let get outside defended =
    let o = Bool.to_int outside in
    let d = Bool.to_int defended in
    uget t (d + o * 2)
end

let behind_pawn = 3 $ 28

(* By distance from either king (using the Chebyshev method). *)
let king_distance = [|
  -9 $ -6;
  -12 $ -20;
  -27 $ -20;
  -47 $ -19;
|]

(* By number of attacked squares in our mobility area *)
let mobility = [|
  -104 $ -139;
  -45 $ -114;
  -22 $ -37;
  -8 $ 3;
  6 $ 15;
  11 $ 34;
  19 $ 38;
  30 $ 37;
  43 $ 17;
|]

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
  let our_knights = knight & us in
  let their_pawns = pawn & them in
  let outpost = Pre.outpost_ranks c in
  let adv = pawn_advance c' pawn in
  Bb.fold our_knights ~init:(0 $ 0) ~f:(fun acc sq ->
      let att = Pre.knight sq in
      Eval_info.update_attacks our_info att Knight;
      (* Knight outpost. *)
      let acc =
        let os = Pre.outpost_squares sq c in
        if sq @ outpost && (os & their_pawns) = empty then
          let outside = sq @ (file_a + file_h) in
          let defended = sq @ our_info.pawn_attacks in
          acc +$ Outpost.get outside defended
        else acc in
      (* Behind pawn. *)
      let acc = if sq @ adv then acc +$ behind_pawn else acc in
      (* Distance from king. *)
      let acc =
        let ours = Square.chebyshev sq our_info.king_square in
        let theirs = Square.chebyshev sq their_info.king_square in
        let d = Int.(imin ours theirs - 4) in
        if Int.(d < 0) then acc else acc +$ uget king_distance d in
      (* Mobility. *)
      let acc =
        let c = count (our_info.mobility_area & att) in
        acc +$ uget mobility c in
      (* King safety. *)
      Eval_info.update_king_safety their_info att Eval_safety.knight;
      acc)
