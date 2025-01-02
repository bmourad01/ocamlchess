(* Evaluation of bishop placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

module Outpost = struct
  let t = [|
    16 $ -16;
    50 $ -3;
    9 $ -9;
    -4 $ -4;
  |]

  let get outside defended =
    let o = Bool.to_int outside in
    let d = Bool.to_int defended in
    uget t (d + o * 2)
end

let pair = 22 $ 88
let rammed_pawn = -8 $ -17
let behind_pawn = 4 $ 24
let long_diagonal = 26 $ 20

(* By number of attacked squares in our mobility area *)
let mobility = [|
  -99 $ -186;
  -46 $ -124;
  -16 $ -54;
  -4 $ -14;
  6 $ 1;
  14 $ 20;
  17 $ 35;
  19 $ 39;
  19 $ 49;
  27 $ 48;
  26 $ 48;
  52 $ 32;
  55 $ 47;
  83 $ 2;
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
  let bishop = Position.bishop info.pos in
  let our_bishops = bishop & us in
  let their_pawns = pawn & them in
  let orank = Pre.outpost_ranks c in
  let adv = pawn_advance c' pawn in
  (* Bishop pair. *)
  let init =
    if (our_bishops & white) <> empty
    && (our_bishops & black) <> empty
    then pair else 0 $ 0 in
  Bb.fold our_bishops ~init ~f:(fun acc sq ->
      let att = Pre.bishop sq our_info.no_bishop in
      Eval_info.update_attacks our_info att Bishop;
      (* Rammed pawns. *)
      let acc =
        let m = if sq @ white then white else black in
        let c = count (our_info.rammed_pawns & m) in
        acc +$ (rammed_pawn ** c) in
      (* Bishop outpost. *)
      let acc =
        let osq = Pre.outpost_squares sq c in
        if sq @ orank && (osq & their_pawns) = empty then
          let outside = sq @ (file_a + file_h) in
          let defended = sq @ our_info.pawn_attacks in
          acc +$ Outpost.get outside defended
        else acc in
      (* Behind pawn. *)
      let acc = if sq @ adv then acc +$ behind_pawn else acc in
      (* Control center on long diagonal. *)
      let acc =
        let c = Pre.bishop sq pawn & center in
        if sq @ (longdiag - center) && several c
        then acc +$ long_diagonal else acc in
      (* Mobility. *)
      let acc =
        let c = count (our_info.mobility_area & att) in
        acc +$ uget mobility c in
      (* King safety. *)
      Eval_info.update_king_safety their_info att Eval_safety.bishop;
      acc)
