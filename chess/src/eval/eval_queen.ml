(* Evaluation of rook placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

let discovery = -22 $ -13

(* By number of attacked squares in our mobility area *)
let mobility = [|
  -111 $ -273;
  -253 $ -401;
  -127 $ -228;
  -46 $ -236;
  -20 $ -173;
  -9 $ -86;
  -1 $ -35;
  2 $ -1;
  8 $ 8;
  10 $ 31;
  15 $ 37;
  17 $ 55;
  20 $ 46;
  23 $ 57;
  22 $ 58;
  21 $ 64;
  24 $ 62;
  16 $ 65;
  13 $ 63;
  18 $ 48;
  25 $ 30;
  38 $ 8;
  34 $ -12;
  28 $ -29;
  10 $ -44;
  7 $ -79;
  -42 $ -30;
  -23 $ -50;
|]

let[@inline] go info c =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let bishop = Position.bishop info.pos in
  let rook = Position.rook info.pos in
  let queen = Position.queen info.pos in
  let our_queens = queen & us in
  let their_bishops = bishop & them in
  let their_rooks = rook & them in
  Bb.fold our_queens ~init:(0 $ 0) ~f:(fun acc sq ->
      let att = Pre.queen sq all in
      Eval_info.update_attacks our_info att Queen;
      (* Discovered attacks. *)
      let acc =
        (* Remove bishops and rooks that can be reached by the queen. *)
        let batt = Pre.bishop sq all in
        let ratt = Pre.rook sq all in
        let b = their_bishops - batt in
        let r = their_rooks - ratt in
        (* See if there is a second bishop or rook in the attack. *)
        if (b & Pre.bishop sq (all - batt)) <> empty
        || (r & Pre.rook   sq (all - ratt)) <> empty
        then acc +$ discovery else acc in
      (* Mobility. *)
      let acc =
        let c = count (our_info.mobility_area & att) in
        acc +$ uget mobility c in
      (* King safety. *)
      Eval_info.update_king_safety their_info att Eval_safety.queen;
      acc)
