(* Evaluation of passed pawns. *)

open Core_kernel [@@warning "-D"]
open Eval_common
open Score.Syntax

module Rank = struct
  let t = [|
    (* Cannot advance, unsafe. *)
    0 $ 0;
    -39 $ -4;
    -43 $ 25;
    -62 $ 28;
    8 $ 19;
    97 $ -4;
    162 $ 46;
    0 $ 0;
    (* Cannot advance, safe. *)
    0 $ 0;
    -28 $ 13;
    -40 $ 42;
    -56 $ 44;
    -2 $ 56;
    114 $ 54;
    193 $ 94;
    0 $ 0;
    (* Can advance, unsafe. *)
    0 $ 0;
    -28 $ 29;
    -47 $ 36;
    -60 $ 54;
    8 $ 65;
    106 $ 76;
    258 $ 124;
    0 $ 0;
    (* Can advance, safe. *)
    0 $ 0;
    -28 $ 23;
    -40 $ 35;
    -55 $ 60;
    8 $ 89;
    95 $ 166;
    124 $ 293;
    0 $ 0;
  |]

  let get can_advance safe_advance rank =
    let rc = Square.Rank.count in
    let c = Bool.to_int can_advance in
    let s = Bool.to_int safe_advance in
    let i = rank + (rc * s) + (rc * 2 * c) in
    uget t i
end

module Distance = struct
  (* By distance from king (using the Chebyshev method). *)
  let ours = [|
    0 $ 0;
    -3 $ 1;
    0 $ -4;
    5 $ -13;
    6 $ -19;
    -9 $ -19;
    -9 $ -7;
    0 $ 0;
  |]

  (* By distance from king (using the Chebyshev method). *)
  let theirs = [|
    0 $ 0;
    5 $ -1;
    7 $ 0;
    9 $ 11;
    0 $ 25;
    1 $ 37;
    16 $ 37;
    0 $ 0;
  |]
end

let promotion = -49 $ 56

let[@inline] go info c =
  let open Bb in
  let c' = Piece.Color.opposite c in
  let our_info = Eval_info.color info c in
  let their_info = Eval_info.color info c' in
  let all = Position.all_board info.pos in
  let us = Position.board_of_color info.pos c in
  let them = all - us in
  let pawn = Position.pawn info.pos in
  let our_pawns = pawn & us in
  let relr = relative_rank c in
  let fwd = square_fwd c in
  Bb.fold our_pawns ~init:(0 $ 0) ~f:(fun acc sq ->
      let r = relr sq and b = !!(fwd sq) in
      (* Rank. *)
      let acc =
        let can_advance = (b & all) = empty in
        let safe_advance = (b & their_info.attacks) = empty in
        Rank.get can_advance safe_advance r in
      (* Ignore additional passers. *)
      if not (several (Pre.forward_files sq c & our_info.passed_pawns)) then
        (* Distance from our king. *)
        let acc =
          let d = Square.chebyshev sq our_info.king_square in
          acc +$ uget Distance.ours d in
        (* Distance from their king. *)
        let acc =
          let d = Square.chebyshev sq their_info.king_square in
          acc +$ uget Distance.theirs d in
        (* Promotion. *)
        let acc =
          let f = Square.file sq in
          let b = Pre.forward_ranks sq c & file_exn f in
          if (b & (them + their_info.attacks)) = empty
          then acc +$ promotion else acc in
        acc
      else acc)
