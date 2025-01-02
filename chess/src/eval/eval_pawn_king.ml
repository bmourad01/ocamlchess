(* Evaluation of combined pawn and king placement. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

(* By king-pawn file distance. *)
let proximity = [|
  36 $ 46;
  22 $ 31;
  13 $ 15;
  -8 $ -22;
  -5 $ -62;
  -3 $ -75;
  -15 $ -81;
  -12 $ -75;
|]

module Shelter = struct
  let t = [|
    (* Different file. *)
    (* A *)
    -5 $ -5;
    17 $ -31;
    26 $ -3;
    24 $ 8;
    4 $ 1;
    -12 $ 4;
    -16 $ -33;
    -59 $ 24;
    (* B *)
    11 $ -6;
    3 $ -15;
    -5 $ -2;
    5 $ -4;
    -11 $ 7;
    -53 $ 70;
    81 $ 82;
    -19 $ 1;
    (* C *)
    38 $ -3;
    5 $ -6;
    -34 $ 5;
    -17 $ -15;
    -9 $ -5;
    -26 $ 12;
    11 $ 73;
    -16 $ -1;
    (* D *)
    18 $ 11;
    25 $ -18;
    0 $ -14;
    10 $ -21;
    22 $ -34;
    -48 $ 9;
    -140 $ 49;
    -5 $ -5;
    (* E *)
    -11 $ 15;
    1 $ -3;
    -44 $ 6;
    -28 $ 10;
    -24 $ -2;
    -35 $ -5;
    40 $ -24;
    -13 $ 3;
    (* F *)
    51 $ -14;
    15 $ -14;
    -24 $ 5;
    -10 $ -20;
    10 $ -34;
    34 $ -20;
    48 $ -38;
    -21 $ 1;
    (* G *)
    40 $ -17;
    2 $ -24;
    -31 $ -1;
    -24 $ -8;
    -31 $ 2;
    -20 $ 29;
    4 $ 49;
    -16 $ 3;
    (* H *)
    10 $ -20;
    4 $ -24;
    10 $ 2;
    2 $ 16;
    -10 $ 24;
    -10 $ 44;
    -184 $ 81;
    -17 $ 17;
    (* Same file. *)
    (* A *)
    0 $ 0;
    -15 $ -39;
    9 $ -29;
    -49 $ 14;
    -36 $ 6;
    -8 $ 50;
    -168 $ -3;
    -59 $ 19;
    (* B *)
    0 $ 0;
    17 $ -18;
    9 $ -11;
    -11 $ -5;
    -1 $ -24;
    26 $ 73;
    -186 $ 4;
    -32 $ 11;
    (* C *)
    0 $ 0;
    19 $ -9;
    1 $ -11;
    9 $ -26;
    28 $ -5;
    -92 $ 56;
    -88 $ -74;
    -8 $ 1;
    (* D *)
    0 $ 0;
    0 $ 3;
    -6 $ -6;
    -35 $ 10;
    -46 $ 13;
    -98 $ 33;
    -7 $ -45;
    -35 $ -5;
    (* E *)
    0 $ 0;
    12 $ -3;
    17 $ -15;
    17 $ -15;
    -5 $ -14;
    -36 $ 5;
    -101 $ -52;
    -18 $ -1;
    (* F *)
    0 $ 0;
    -8 $ -5;
    -22 $ 1;
    -16 $ -6;
    25 $ -22;
    -27 $ 10;
    52 $ 39;
    -14 $ -2;
    (* G *)
    0 $ 0;
    32 $ -22;
    19 $ -15;
    -9 $ -6;
    -29 $ 13;
    -7 $ 23;
    -50 $ -39;
    -27 $ 18;
    (* H *)
    0 $ 0;
    16 $ -57;
    17 $ -32;
    -18 $ -7;
    -31 $ 24;
    -11 $ 24;
    -225 $ -49;
    -30 $ 5;
  |]

  let get same_file f r =
    let rc = Square.Rank.count in
    let fc = Square.File.count in
    let i = r + (f * rc) + (Bool.to_int same_file * fc * rc) in
    uget t i
end

module Storm = struct
  let t = [|
    (* Not blocked *)
    (* +0 *)
    -6 $ 36;
    144 $ -4;
    -13 $ 26;
    -7 $ 1;
    -12 $ -3;
    -8 $ -7;
    -19 $ 8;
    -28 $ -2;
    (* +1 *)
    -17 $ 60;
    64 $ 17;
    -9 $ 21;
    8 $ 12;
    3 $ 9;
    6 $ -2;
    -5 $ 2;
    -16 $ 8;
    (* +2 *)
    2 $ 48;
    15 $ 30;
    -17 $ 20;
    -13 $ 10;
    -1 $ 6;
    7 $ 3;
    8 $ -7;
    7 $ 8;
    (* +3 *)
    -1 $ 25;
    15 $ 22;
    -31 $ 10;
    -22 $ 1;
    -15 $ 4;
    13 $ -10;
    3 $ 5;
    -20 $ 8;
    (* Blocked *)
    (* +0 *)
    0 $ 0;
    -18 $ 16;
    -18 $ -2;
    27 $ -24;
    10 $ -6;
    15 $ -24;
    -6 $ 9;
    9 $ 30;
    (* +1 *)
    0 $ 0;
    -15 $ -24;
    -3 $ -15;
    53 $ -17;
    15 $ -5;
    20 $ -28;
    -12 $ -17;
    -34 $ 5;
    (* +2 *)
    0 $ 0;
    -34 $ -62;
    -15 $ -13;
    9 $ -6;
    6 $ -2;
    -2 $ -17;
    -5 $ -21;
    -3 $ 3;
    (* +3 *)
    0 $ 0;
    -1 $ -26;
    -27 $ -18;
    -21 $ 4;
    -10 $ -6;
    7 $ -35;
    66 $ -29;
    11 $ 25;
  |]

  let get blocked file rank =
    let rc = Square.Rank.count in
    let mf = Square.File.mirror_exn file in
    let i = rank + (mf * rc) + (Bool.to_int blocked * 4 * rc) in
    uget t i
end

let surrounding_files f =
  let fa = Int.(imax 0 (f - 1)) in
  let fb = Int.(imin (Square.File.count - 1) (f + 1)) in
  Sequence.range fa fb ~stop:`inclusive

let[@inline] go info c =
  let open Bb in
  let our_info = Eval_info.color info c in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let our_pawns = pawn & us in
  let their_pawns = pawn & them in
  let kr, kf = Square.decomp our_info.king_square in
  let fwd = Pre.forward_ranks our_info.king_square c in
  let bmost = Fn.compose Square.rank @@ backmost c in
  (* Proximity of king to nearest file-wise pawn. *)
  let dist = Pre.king_pawn_file_distance our_info.king_square pawn in
  our_info.pawn_king_eval <- our_info.pawn_king_eval +$ uget proximity dist;
  (* Look at the files of and surrounding our king. *)
  surrounding_files kf |> Sequence.iter ~f:(fun f ->
      let eval = our_info.pawn_king_eval in
      let safety = our_info.pawn_king_safety in
      (* Closest firendly and enemy pawns at or above the king
         on this file. *)
      let fb = file_exn f in
      let ours = our_pawns & fb & fwd in
      let theirs = their_pawns & fb & fwd in
      (* Rank-wise distance between these pawns and our king. If the pawns
         are missing then a distance of 7 is used, since if there were a
         pawn it would be strictly less than 7. *)
      let od = if ours   <> empty then iabs Int.(kr - bmost ours)   else 7 in
      let td = if theirs <> empty then iabs Int.(kr - bmost theirs) else 7 in
      (* Shelter. *)
      let same_file = Int.(f = kf) in
      let eval = eval +$ Shelter.get same_file f od in
      let safety = safety +$ Eval_safety.Shelter.get same_file od in
      (* Storm. *)
      let blocked = Int.(od <> 7 && (od = td - 1)) in
      let eval = eval +$ Storm.get blocked f td in
      let safety = safety +$ Eval_safety.Storm.get blocked td in
      (* Update. *)
      our_info.pawn_king_eval <- eval;
      our_info.pawn_king_safety <- safety)
